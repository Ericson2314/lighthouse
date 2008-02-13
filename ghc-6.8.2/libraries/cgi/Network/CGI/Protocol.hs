-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Protocol
-- Copyright   :  (c) Bjorn Bringert 2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- An implementation of the program side of the CGI protocol.
--
-----------------------------------------------------------------------------

module Network.CGI.Protocol (
  -- * CGI request
  CGIRequest(..), Input(..), 
  -- * CGI response
  CGIResult(..),
  Headers, HeaderName(..),
  -- * Running CGI actions
  hRunCGI, runCGIEnvFPS,
  -- * Inputs
  decodeInput, takeInput,
  -- * Environment variables
  getCGIVars,
  -- * Logging
  logCGI,
  -- * URL encoding
  formEncode, urlEncode, formDecode, urlDecode,
  -- * Utilities
  maybeRead, replace
 ) where

import Control.Monad.Trans (MonadIO(..))
import Data.Char (toLower)
import Data.List (intersperse)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Network.URI (unEscapeString,escapeURIString,isUnescapedInURI)
import System.Environment (getEnvironment)
import System.IO (Handle, hPutStrLn, stderr, hFlush)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

import Data.Typeable (Typeable(..), mkTyConApp, mkTyCon)

import Network.CGI.Multipart



--
-- * CGI request
--

-- | The input to a CGI action.
data CGIRequest = 
    CGIRequest {
                -- | Environment variables.
                cgiVars :: Map String String,
                -- | Input parameters. For better laziness in reading inputs,
                --   this is not a Map.
                cgiInputs :: [(String, Input)],
                -- | Raw request body. To avoid memory leaks,
                -- this is the empty string if the request body has been
                -- interpreted as inputs in
                -- "application\/x-www-form-urlencoded" or
                -- "multipart\/form-data" format.
                cgiRequestBody :: ByteString
               }
    deriving (Show)

instance Typeable CGIResult where
    typeOf _ = mkTyConApp (mkTyCon "Network.CGI.Protocol.CGIResult") []

-- | The value of an input parameter, and some metadata.
data Input = Input {
                    inputValue :: ByteString,
                    inputFilename :: Maybe String,
                    inputContentType :: ContentType
                   }
              deriving Show

--
-- * CGI response
--

-- | The result of a CGI program.
data CGIResult = CGIOutput ByteString
               | CGINothing
                 deriving (Show, Read, Eq, Ord)

type Headers = [(HeaderName, String)]

-- | A string with case insensitive equality and comparisons.
newtype HeaderName = HeaderName String deriving (Show)

instance Eq HeaderName where
    HeaderName x == HeaderName y = map toLower x == map toLower y

instance Ord HeaderName where
    HeaderName x `compare` HeaderName y = map toLower x `compare` map toLower y



--
-- * Running CGI actions
--

-- | Runs a CGI action in a given environment. Uses Handles for input and output. 
hRunCGI :: MonadIO m =>
           [(String,String)] -- ^ CGI environment variables, e.g. from 'getCGIVars'.
        -> Handle -- ^ Handle that input will be read from, e.g. 'System.IO.stdin'.
        -> Handle -- ^ Handle that output will be written to, e.g. 'System.IO.stdout'.
        -> (CGIRequest -> m (Headers, CGIResult)) -- ^ CGI action
        -> m ()
hRunCGI env hin hout f = 
    do inp <- liftIO $ BS.hGetContents hin
       outp <- runCGIEnvFPS env inp f
       liftIO $ BS.hPut hout outp
       liftIO $ hFlush hout

-- | Runs a CGI action in a given environment. Uses lazy ByteStrings 
--   for input and output.
runCGIEnvFPS :: Monad m =>
             [(String,String)] -- ^ CGI environment variables.
          -> ByteString -- ^ Request body.
          -> (CGIRequest -> m (Headers, CGIResult)) -- ^ CGI action.
          -> m ByteString -- ^ Response (headers and content).
runCGIEnvFPS vars inp f
    = do let (inputs,body) = decodeInput vars inp
         (hs,outp) <- f $ CGIRequest {
                                      cgiVars = Map.fromList vars,
                                      cgiInputs = inputs,
                                      cgiRequestBody = body
                                     }
         return $ case outp of
           CGIOutput c -> formatResponse c hs'
               where hs' = if isJust (lookup ct hs)
                              then hs else hs ++ [(ct,defaultContentType)]
                     ct = HeaderName "Content-type"
           CGINothing -> formatResponse BS.empty hs

formatResponse :: ByteString -> Headers -> ByteString
formatResponse c hs = 
    -- NOTE: we use CRLF since lighttpd mod_fastcgi can't handle
    -- just LF if there are CRs in the content.
    unlinesCrLf ([BS.pack (n++": "++v) | (HeaderName n,v) <- hs] 
                ++ [BS.empty,c])
  where unlinesCrLf = BS.concat . intersperse (BS.pack "\r\n")

defaultContentType :: String
defaultContentType = "text/html; charset=ISO-8859-1"


--
-- * Inputs
--


-- | Gets and decodes the input according to the request
--   method and the content-type.
decodeInput :: [(String,String)] -- ^ CGI environment variables.
            -> ByteString        -- ^ Request body.
            -> ([(String,Input)],ByteString)  
               -- ^ A list of input variables and values, and the request body
               -- if it was not interpreted.
decodeInput env inp =
  let (inputs, body) = bodyInput env inp in (queryInput env ++ inputs, body)

-- | Builds an 'Input' object for a simple value.
simpleInput :: String -> Input
simpleInput v = Input { inputValue = BS.pack v,
                        inputFilename = Nothing,
                        inputContentType = defaultInputType }

-- | The default content-type for variables.
defaultInputType :: ContentType
defaultInputType = ContentType "text" "plain" [] -- FIXME: use some default encoding?

--
-- * Environment variables
--

-- | Gets the values of all CGI variables from the program environment.
getCGIVars :: MonadIO m => m [(String,String)]
getCGIVars = liftIO getEnvironment

--
-- * Logging
--

-- | Logs some message using the server\'s logging facility.
-- FIXME: does this have to be more general to support
-- FastCGI etc? Maybe we should store log messages in the
-- CGIState?
logCGI :: MonadIO m => String -> m ()
logCGI s = liftIO (hPutStrLn stderr s)

--
-- * Query string
--

-- | Gets inputs from the query string.
queryInput :: [(String,String)] -- ^ CGI environment variables.
           -> [(String,Input)] -- ^ Input variables and values.
queryInput env = formInput $ lookupOrNil "QUERY_STRING" env

-- | Decodes application\/x-www-form-urlencoded inputs.
formInput :: String
          -> [(String,Input)] -- ^ Input variables and values.
formInput qs = [(n, simpleInput v) | (n,v) <- formDecode qs]

--
-- * URL encoding
--

-- | Formats name-value pairs as application\/x-www-form-urlencoded.
formEncode :: [(String,String)] -> String
formEncode xs = 
    concat $ intersperse "&" [urlEncode n ++ "=" ++ urlEncode v | (n,v) <- xs]

-- | Converts a single value to the application\/x-www-form-urlencoded encoding.
urlEncode :: String -> String
urlEncode = replace ' ' '+' . escapeURIString okChar
  where okChar c = c == ' ' || 
                   (isUnescapedInURI c && c `notElem` "&=+")

-- | Gets the name-value pairs from application\/x-www-form-urlencoded data.
formDecode :: String -> [(String,String)]
formDecode "" = []
formDecode s = (urlDecode n, urlDecode (drop 1 v)) : formDecode (drop 1 rs)
    where (nv,rs) = break (=='&') s
          (n,v) = break (=='=') nv

-- | Converts a single value from the 
--   application\/x-www-form-urlencoded encoding.
urlDecode :: String -> String
urlDecode = unEscapeString . replace '+' ' '

--
-- * Request content and form-data stuff
--

-- | Gets input variables from the body, if any.
bodyInput :: [(String,String)]
          -> ByteString
          -> ([(String,Input)], ByteString)
bodyInput env inp =
   case lookup "REQUEST_METHOD" env of
      Just "POST" -> 
          let ctype = lookup "CONTENT_TYPE" env >>= parseContentType
           in decodeBody ctype $ takeInput env inp
      _ -> ([], inp)

-- | Decodes a POST body.
decodeBody :: Maybe ContentType
           -> ByteString
           -> ([(String,Input)], ByteString)
decodeBody ctype inp = 
    case ctype of
               Just (ContentType "application" "x-www-form-urlencoded" _) 
                   -> (formInput (BS.unpack inp), BS.empty)
               Just (ContentType "multipart" "form-data" ps) 
                   -> (multipartDecode ps inp, BS.empty)
               Just _ -> ([], inp) -- unknown content-type, the user will have to
                            -- deal with it by looking at the raw content
               -- No content-type given, assume x-www-form-urlencoded
               Nothing -> (formInput (BS.unpack inp), BS.empty)

-- | Takes the right number of bytes from the input.
takeInput :: [(String,String)]  -- ^ CGI environment variables.
          -> ByteString         -- ^ Request body.
          -> ByteString         -- ^ CONTENT_LENGTH bytes from the request 
                                --   body, or the empty string if there is no
                                --   CONTENT_LENGTH.
takeInput env req = 
    case len of
           Just l  -> BS.take l req
           Nothing -> BS.empty
     where len = lookup "CONTENT_LENGTH" env >>= maybeRead

-- | Decodes multipart\/form-data input.
multipartDecode :: [(String,String)] -- ^ Content-type parameters
                -> ByteString        -- ^ Request body
                -> [(String,Input)]  -- ^ Input variables and values.
multipartDecode ps inp =
    case lookup "boundary" ps of
         Just b -> let MultiPart bs = parseMultipartBody b inp
                    in map bodyPartToInput bs
         Nothing -> [] -- FIXME: report that there was no boundary

bodyPartToInput :: BodyPart -> (String,Input)
bodyPartToInput (BodyPart hs b) = 
    case getContentDisposition hs of
              Just (ContentDisposition "form-data" ps) -> 
                  (lookupOrNil "name" ps,
                   Input { inputValue = b,
                           inputFilename = lookup "filename" ps,
                           inputContentType = ctype })
              _ -> ("ERROR",simpleInput "ERROR") -- FIXME: report error
    where ctype = fromMaybe defaultInputType (getContentType hs)


--
-- * Utilities
--

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Same as 'lookup' specialized to strings, but 
--   returns the empty string if lookup fails.
lookupOrNil :: String -> [(String,String)] -> String
lookupOrNil n = fromMaybe "" . lookup n

