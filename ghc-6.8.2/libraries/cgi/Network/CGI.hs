{-# OPTIONS_GHC -fallow-overlapping-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Bjorn Bringert 2004-2006
--                (c) Ian Lynagh 2005
--                (c) Jeremy Shaw 2005
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable (uses Control.Monad.State)
--
-- Simple Library for writing CGI programs.
-- See <http://hoohoo.ncsa.uiuc.edu/cgi/interface.html> for the
-- CGI specification.
--
-- This version of the library is for systems with version 2.0 or greater
-- of the network package. This includes GHC 6.6 and later. For older
-- systems, see <http://www.cs.chalmers.se/~bringert/darcs/cgi-compat/doc/>
--
-- Based on the original Haskell binding for CGI:
--
-- Original Version by Erik Meijer <mailto:erik@cs.ruu.nl>.
-- Further hacked on by Sven Panne <mailto:sven.panne@aedion.de>.
-- Further hacking by Andy Gill <mailto:andy@galconn.com>.
-- A new, hopefully more flexible, interface
-- and support for file uploads by Bjorn Bringert <mailto:bjorn@bringert.net>.
--
-- Here is a simple example, including error handling (not that there is 
-- much that can go wrong with Hello World):
--
-- > import Network.CGI
-- >
-- > cgiMain :: CGI CGIResult
-- > cgiMain = output "Hello World!"
-- >
-- > main :: IO ()
-- > main = runCGI (handleErrors cgiMain)
--
--
-----------------------------------------------------------------------------

module Network.CGI (
  -- * CGI monad
    MonadCGI, CGIT, CGIResult, CGI
  , MonadIO, liftIO
  , runCGI
  -- * Error handling
  , throwCGI, catchCGI, tryCGI, handleExceptionCGI
  , handleErrors
  -- * Logging
  , logCGI
  -- * Output
  , output, outputFPS, outputNothing, redirect
  , setHeader, setStatus
  -- * Error pages
  , outputError, outputException 
  , outputNotFound, outputMethodNotAllowed, outputInternalServerError
  -- * Input
  , getInput, getInputFPS, readInput
  , getInputs, getInputNames
  , getMultiInput
  , getInputFilename, getInputContentType
  -- * Environment
  , getVar, getVarWithDefault, getVars
  , serverName, serverPort
  , requestMethod, pathInfo
  , pathTranslated, scriptName
  , queryString
  , remoteHost, remoteAddr
  , authType, remoteUser
  , requestContentType, requestContentLength
  , requestHeader
  -- * Program and request URI
  , progURI, queryURI, requestURI
  -- * Content type
  , ContentType(..), showContentType, parseContentType
  -- * Cookies
  , Cookie(..), newCookie
  , getCookie, readCookie
  , setCookie, deleteCookie
  -- * URL encoding
  , formEncode, urlEncode, formDecode, urlDecode
  ) where

import Control.Exception (Exception(..))
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Char (toUpper)
import Data.List (intersperse, sort, group)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Network.URI (URI(..), URIAuth(..), nullURI, parseRelativeReference, 
                    escapeURIString, isUnescapedInURI)
import System.IO (stdin, stdout)
import System.IO.Error (isUserError, ioeGetErrorString)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

import Network.CGI.Cookie (Cookie(..), showCookie, newCookie, findCookie)
import qualified Network.CGI.Cookie as Cookie (deleteCookie)
import Network.CGI.RFC822Headers (ContentType(..), 
                                  parseContentType, showContentType)
import Network.CGI.Monad
import Network.CGI.Protocol

import Text.XHtml (Html, renderHtml, header, (<<), thetitle, (+++), 
                   body, h1, paragraph, hr, address)


-- | Run a CGI action. Typically called by the main function.
--   Reads input from stdin and writes to stdout. Gets
--   CGI environment variables from the program environment.
runCGI :: MonadIO m => CGIT m CGIResult -> m ()
runCGI f = do env <- getCGIVars
              hRunCGI env stdin stdout (runCGIT f)


--
-- * Output \/ redirect
--

-- | Output a 'String'. The output is assumed to be text\/html, encoded using
--   ISO-8859-1. To change this, set the Content-type header using
--   'setHeader'.
output :: MonadCGI m =>
          String        -- ^ The string to output.
       -> m CGIResult
output = return . CGIOutput . BS.pack

-- | Output a 'ByteString'. The output is assumed to be text\/html, 
--   encoded using ISO-8859-1. To change this, set the 
--   Content-type header using 'setHeader'.
outputFPS :: MonadCGI m =>
             ByteString        -- ^ The string to output.
          -> m CGIResult
outputFPS = return . CGIOutput

-- | Do not output anything (except headers).
outputNothing :: MonadCGI m => m CGIResult
outputNothing = return CGINothing

-- | Redirect to some location.
redirect :: MonadCGI m =>
            String        -- ^ A URL to redirect to.
         -> m CGIResult
redirect url = do setHeader "Location" url
                  outputNothing

--
-- * Error handling
--

-- | Catches any exception thrown by the given CGI action,
--   returns an error page with a 500 Internal Server Error,
--   showing the exception information, and logs the error.
--   
--   Typical usage:
--
-- > cgiMain :: CGI CGIResult
-- > cgiMain = ...
-- >
-- > main :: IO ()
-- > main = runCGI (handleErrors cgiMain)
handleErrors :: CGI CGIResult -> CGI CGIResult
handleErrors = flip catchCGI outputException

--
-- * Error output
--

-- | Output a 500 Internal Server Error with information from
--   an 'Exception'.
outputException :: (MonadCGI m,MonadIO m) => Exception -> m CGIResult
outputException e = outputInternalServerError es
    where es = case e of
                 ErrorCall msg  -> [msg]
                 IOException ie -> ioe ie
                 _              -> [show e]
          ioe ie = if isUserError ie then [ioeGetErrorString ie] else [show ie]

-- | Output an error page to the user, with the given
--   HTTP status code in the response. Also logs the error information
--   using 'logCGI'.
outputError :: (MonadCGI m, MonadIO m) =>
               Int      -- ^ HTTP Status code
            -> String   -- ^ Status message
            -> [String] -- ^ Error information
            -> m CGIResult
outputError c m es = 
      do logCGI $ show (c,m,es)
         setStatus c m
         setHeader "Content-type" "text/html; charset=ISO-8859-1"
         page <- errorPage c m es 
         output $ renderHtml page

-- | Create an HTML error page.
errorPage :: MonadCGI m => 
             Int      -- ^ Status code
          -> String   -- ^ Status message
          -> [String] -- ^ Error information
          -> m Html
errorPage c m es = 
    do server <- getVar "SERVER_SOFTWARE"
       host   <- getVar "SERVER_NAME"
       port   <- getVar "SERVER_PORT"
       let tit = show c ++ " " ++ m
           sig = "Haskell CGI" 
                 ++ " on " ++ fromMaybe "" server
                 ++ " at " ++ fromMaybe "" host ++ maybe "" (", port "++) port
       return $ header << thetitle << tit 
                  +++ body << (h1 << tit +++ map (paragraph <<) es 
                               +++ hr +++ address << sig)

--
-- * Specific HTTP errors
--

-- | Use 'outputError' to output and log a 404 Not Found error.
outputNotFound :: (MonadIO m, MonadCGI m) => 
                 String -- ^ The name of the requested resource.
              -> m CGIResult
outputNotFound r =
    outputError 404 "Not Found" ["The requested resource was not found: " ++ r]

-- | Use 'outputError' to output and log a 405 Method Not Allowed error.
outputMethodNotAllowed :: (MonadIO m, MonadCGI m) => 
                          [String] -- ^ The allowed methods.
                       -> m CGIResult
outputMethodNotAllowed ms = 
    do let allow = concat $ intersperse ", " ms
       setHeader "Allow" allow
       outputError 405 "Method Not Allowed" ["Allowed methods : " ++ allow]

-- | Use 'outputError' to output and log a 500 Internal Server Error.
outputInternalServerError :: (MonadIO m, MonadCGI m) =>
                             [String] -- ^ Error information.
                          -> m CGIResult
outputInternalServerError es = outputError 500 "Internal Server Error" es


--
-- * Environment variables
--

-- | Get the value of a CGI environment variable. Example:
--
-- > remoteAddr <- getVar "REMOTE_ADDR"
getVar :: MonadCGI m =>
          String             -- ^ The name of the variable.
       -> m (Maybe String)
getVar name = liftM (Map.lookup name) $ cgiGet cgiVars

getVarWithDefault :: MonadCGI m =>
                     String -- ^ The name of the variable.
                  -> String -- ^ Default value 
                  -> m String
getVarWithDefault name def = liftM (fromMaybe def) $ getVar name

-- | Get all CGI environment variables and their values.
getVars :: MonadCGI m =>
           m [(String,String)]
getVars = liftM Map.toList $ cgiGet cgiVars


-- | The server\'s hostname, DNS alias, or IP address as it would 
--   appear in self-referencing URLs.
serverName :: MonadCGI m => m String
serverName = getVarWithDefault "SERVER_NAME" ""

-- | The port number to which the request was sent.
serverPort :: MonadCGI m => m Int
serverPort = liftM (fromMaybe 80 . (>>= maybeRead)) (getVar "SERVER_PORT")

-- |  The method with which the request was made. 
--    For HTTP, this is \"GET\", \"HEAD\", \"POST\", etc.
requestMethod :: MonadCGI m => m String
requestMethod = getVarWithDefault "REQUEST_METHOD" "GET"

-- | The extra path information, as given by the client.
--   This is any part of the request path that follows the
--   CGI program path.
--   If the string returned by this function is not empty,
--   it is guaranteed to start with a @\'\/\'@.
--
-- Note that this function returns an unencoded string.
-- Make sure to percent-encode any characters
-- that are not allowed in URI paths before using the result of
-- this function to construct a URI.
-- See 'progURI', 'queryURI' and 'requestURI' for a higher-level
-- interface.
pathInfo :: MonadCGI m => m String
pathInfo = liftM slash $ getVarWithDefault "PATH_INFO" ""
  where slash s = if not (null s) && head s /= '/' then '/':s else s

-- | The path returned by 'pathInfo', but with virtual-to-physical
--   mapping applied to it.
pathTranslated :: MonadCGI m => m String
pathTranslated = getVarWithDefault "PATH_TRANSLATED" ""

-- | A virtual path to the script being executed,  
-- used for self-referencing URIs. 
--
-- Note that this function returns an unencoded string.
-- Make sure to percent-encode any characters
-- that are not allowed in URI paths before using the result of
-- this function to construct a URI.
-- See 'progURI', 'queryURI' and 'requestURI' for a higher-level
-- interface.
scriptName :: MonadCGI m => m String
scriptName = getVarWithDefault "SCRIPT_NAME" ""

-- | The information which follows the ? in the URL which referenced 
--   this program. This is the percent-encoded query information.
--   For most normal uses, 'getInput' and friends are probably
--   more convenient.
queryString :: MonadCGI m => m String
queryString = getVarWithDefault "QUERY_STRING" ""

-- | The hostname making the request. If the server does not have
--   this information, Nothing is returned. See also 'remoteAddr'.
remoteHost :: MonadCGI m => m (Maybe String)
remoteHost = getVar "REMOTE_HOST"

-- | The IP address of the remote host making the request.
remoteAddr :: MonadCGI m => m String
remoteAddr = getVarWithDefault "REMOTE_ADDR" ""

-- | If the server supports user authentication, and the script is 
-- protected, this is the protocol-specific authentication method 
-- used to validate the user.
authType :: MonadCGI m => m (Maybe String)
authType = getVar "AUTH_TYPE"

-- | If the server supports user authentication, and the script is 
--   protected, this is the username they have authenticated as.
remoteUser :: MonadCGI m => m (Maybe String)
remoteUser = getVar "REMOTE_USER"

-- | For queries which have attached information, such as 
--   HTTP POST and PUT, this is the content type of the data.
--   You can use 'parseContentType' to get a structured
--   representation of the the content-type value.
requestContentType :: MonadCGI m => m (Maybe String)
requestContentType = getVar "CONTENT_TYPE"

-- | For queries which have attached information, such as 
--   HTTP POST and PUT, this is the length of the content 
--   given by the client.
requestContentLength :: MonadCGI m => m (Maybe Int)
requestContentLength = liftM (>>= maybeRead) $ getVar "CONTENT_LENGTH"

-- | Gets the value of the request header with the given name.
--   The header name is case-insensitive.
--   Example:
--
-- > requestHeader "User-Agent"
requestHeader :: MonadCGI m => String -> m (Maybe String)
requestHeader name = getVar var
  where var = "HTTP_" ++ map toUpper (replace '-' '_' name)


--
-- * Program and request URI
--

-- | Attempts to reconstruct the absolute URI of this program. 
--   This does not include
--   any extra path information or query parameters. See
--   'queryURI' for that.
--   If the server is rewriting request URIs, this URI can
--   be different from the one requested by the client.
--   See also 'requestURI'.
--
-- Characters in the components of the returned URI are escaped 
-- when needed, as required by "Network.URI".
progURI :: MonadCGI m => m URI
progURI =
    do -- Use HTTP_HOST if available, otherwise SERVER_NAME
       h <- requestHeader "Host" >>= maybe serverName return
       p <- serverPort
       name <- scriptName
       https <- liftM (maybe False (const True)) (getVar "HTTPS")
       -- SERVER_PORT might not be the port that the client used
       -- if the server listens on multiple ports, so we give priority
       -- to the port in HTTP_HOST.
       -- HTTP_HOST should include the port according to RFC2616 sec 14.23
       -- Some servers (e.g. lighttpd) also seem to include the port in 
       -- SERVER_NAME. 
       -- We include the port if it is in HTTP_HOST or SERVER_NAME, or if
       -- it is a non-standard port.
       let (host,port) = case break (==':') h of
                           (_,"")  -> (h, if (not https && p == 80) 
                                            || (https && p == 443) 
                                           then "" else ':':show p)
                           (h',p') -> (h',p')
       let auth = URIAuth { uriUserInfo = "", 
                            uriRegName = host,
                            uriPort = port }
       return $ nullURI { uriScheme = if https then "https:" else "http:", 
                          uriAuthority = Just auth,
                          uriPath = escapePath name }

-- | Like 'progURI', but the returned 'URI' also includes
--   any extra path information, and any query parameters.
--   If the server is rewriting request URIs, this URI can
--   be different from the one requested by the client.
--   See also 'requestURI'.
--
-- Characters in the components of the returned URI are escaped 
-- when needed, as required by "Network.URI".
queryURI :: MonadCGI m => m URI
queryURI = 
    do uri  <- progURI
       path <- pathInfo
       qs   <- liftM (\q -> if null q then q else '?':q) $ queryString
       return $ uri { uriPath = uriPath uri ++ escapePath path, 
                      uriQuery = qs } 

-- | Does percent-encoding as needed for URI path components.
escapePath :: String -> String
escapePath = escapeURIString isUnescapedInURIPath
  where isUnescapedInURIPath c = isUnescapedInURI c && c `notElem` "?#"

-- | Attempts to reconstruct the absolute URI requested by the client,
--   including extra path information and query parameters.
--   If no request URI rewriting is done, or if the web server does not
--   provide the information needed to reconstruct the request URI,
--   this function returns the same value as 'queryURI'.
--
-- Characters in the components of the returned URI are escaped 
-- when needed, as required by "Network.URI".
requestURI :: MonadCGI m => m URI
requestURI =
    do uri <- queryURI
       -- Apache sets REQUEST_URI to the original request URI,
       -- with percent-encoding intact.
       mreq <- liftM (>>= parseRelativeReference) $ getVar "REQUEST_URI"
       return $ case mreq of
                 Nothing  -> uri
                 Just req -> uri { 
                                  uriPath  = uriPath req,
                                  uriQuery = uriQuery req
                                 }


--
-- * Inputs
--

-- | Get the value of an input variable, for example from a form.
--   If the variable has multiple values, the first one is returned.
--   Example:
--
-- > query <- getInput "query"
getInput :: MonadCGI m =>
            String           -- ^ The name of the variable.
         -> m (Maybe String) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInput = liftM (fmap BS.unpack) . getInputFPS

-- | Like 'getInput', but returns a 'ByteString'.
getInputFPS :: MonadCGI m =>
            String           -- ^ The name of the variable.
         -> m (Maybe ByteString) -- ^ The value of the variable,
                             --   or Nothing, if it was not set.
getInputFPS = liftM (fmap inputValue) . getInput_

-- | Get all the values of an input variable, for example from a form.
-- This can be used to get all the values from form controls
-- which allow multiple values to be selected.
-- Example:
--
-- > vals <- getMultiInput "my_checkboxes"
getMultiInput :: MonadCGI m => 
                 String -- ^ The name of the variable.
              -> m [String] -- ^ The values of the variable,
                            -- or the empty list if the variable was not set.
getMultiInput n = do is <- cgiGet cgiInputs
                     return [BS.unpack (inputValue v) | (p,v) <- is, p == n]

-- | Get the file name of an input.
getInputFilename :: MonadCGI m =>
                    String           -- ^ The name of the variable.
                 -> m (Maybe String) -- ^ The file name corresponding to the
                                     -- input, if there is one.
getInputFilename = liftM (>>= inputFilename) . getInput_

-- | Get the content-type of an input, if the input exists, e.g. "image\/jpeg".
--   For non-file inputs, this function returns "text\/plain".
--   You can use 'parseContentType' to get a structured
--   representation of the the content-type value.
getInputContentType :: MonadCGI m =>
                       String   -- ^ The name of the variable.
                    -> m (Maybe String) -- ^ The content type, formatted as a string.
getInputContentType = 
    liftM (fmap (showContentType . inputContentType)) . getInput_

-- | Same as 'getInput', but tries to read the value to the desired type.
readInput :: (Read a, MonadCGI m) =>
             String        -- ^ The name of the variable.
          -> m (Maybe a) -- ^ 'Nothing' if the variable does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readInput = liftM (>>= maybeRead) . getInput

-- | Get the names and values of all inputs.
--   Note: the same name may occur more than once in the output,
--   if there are several values for the name.
getInputs :: MonadCGI m => m [(String,String)]
getInputs = do is <- cgiGet cgiInputs
               return [ (n, BS.unpack (inputValue i)) | (n,i) <- is ]

-- | Get the names of all input variables.
getInputNames :: MonadCGI m => m [String]
getInputNames = (sortNub . map fst) `liftM` cgiGet cgiInputs
    where sortNub = map head . group . sort

-- Internal stuff

getInput_ ::  MonadCGI m => String -> m (Maybe Input)
getInput_ n = lookup n `liftM` cgiGet cgiInputs


--
-- * Cookies
--

-- | Get the value of a cookie.
getCookie :: MonadCGI m =>
             String           -- ^ The name of the cookie.
          -> m (Maybe String) -- ^ 'Nothing' if the cookie does not exist.
getCookie name = liftM (>>= findCookie name) (getVar "HTTP_COOKIE")

-- | Same as 'getCookie', but tries to read the value to the desired type.
readCookie :: (Read a, MonadCGI m) =>
              String       -- ^ The name of the cookie.
            -> m (Maybe a) -- ^ 'Nothing' if the cookie does not exist
                           --   or if the value could not be interpreted
                           --   at the desired type.
readCookie = liftM (>>= maybeRead) . getCookie

-- | Set a cookie.
setCookie :: MonadCGI m => Cookie -> m ()
setCookie = setHeader "Set-cookie" . showCookie

-- | Delete a cookie from the client
deleteCookie :: MonadCGI m => Cookie -> m ()
deleteCookie = setCookie . Cookie.deleteCookie


--
-- * Headers
--

-- | Add a response header. 
--   Example:
--
-- > setHeader "Content-type" "text/plain"
setHeader :: MonadCGI m =>
             String -- ^ Header name.
          -> String -- ^ Header value.
          -> m ()
setHeader n v = cgiAddHeader (HeaderName n) v

-- | Set the HTTP response status.
setStatus :: MonadCGI m =>
             Int -- ^  HTTP status code, e.g. @404@
          -> String -- ^ HTTP status message, e.g. @"Not Found"@
          -> m ()
setStatus c m = setHeader "Status" (show c ++ " " ++ m)

