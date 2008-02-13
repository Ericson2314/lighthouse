-- #hide

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.CGI.Multipart
-- Copyright   :  (c) Peter Thiemann 2001,2002
--                (c) Bjorn Bringert 2005-2006
-- License     :  BSD-style
--
-- Maintainer  :  bjorn@bringert.net
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Parsing of the multipart format from RFC2046.
-- Partly based on code from WASHMail.
--
-----------------------------------------------------------------------------
module Network.CGI.Multipart 
    (
     -- * Multi-part messages
     MultiPart(..), BodyPart(..), Header
    , parseMultipartBody, hGetMultipartBody
    , showMultipartBody
     -- * Headers
    , ContentType(..), ContentTransferEncoding(..)
    , ContentDisposition(..)
    , parseContentType
    , parseContentTransferEncoding
    , parseContentDisposition
    , getContentType
    , getContentTransferEncoding
    , getContentDisposition
    ) where

import Control.Monad
import Data.Int (Int64)
import Data.List (intersperse)
import Data.Maybe
import System.IO (Handle)

import Network.CGI.RFC822Headers

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (ByteString)

--
-- * Multi-part stuff.
--

data MultiPart = MultiPart [BodyPart]
               deriving (Show, Read, Eq, Ord)

data BodyPart = BodyPart [Header] ByteString
                deriving (Show, Read, Eq, Ord)

-- | Read a multi-part message from a 'ByteString'.
parseMultipartBody :: String -- ^ Boundary
                   -> ByteString -> MultiPart
parseMultipartBody b = 
    MultiPart . mapMaybe parseBodyPart . splitParts (BS.pack b)

-- | Read a multi-part message from a 'Handle'.
--   Fails on parse errors.
hGetMultipartBody :: String -- ^ Boundary
                  -> Handle
                  -> IO MultiPart
hGetMultipartBody b = liftM (parseMultipartBody b) . BS.hGetContents



parseBodyPart :: ByteString -> Maybe BodyPart
parseBodyPart s =
    do
    let (hdr,bdy) = splitAtEmptyLine s
    hs <- parseM pHeaders "<input>" (BS.unpack hdr)
    return $ BodyPart hs bdy

showMultipartBody :: String -> MultiPart -> ByteString
showMultipartBody b (MultiPart bs) = 
    unlinesCRLF $ foldr (\x xs -> d:showBodyPart x:xs) [c,BS.empty] bs
 where d = BS.pack ("--" ++ b)
       c = BS.pack ("--" ++ b ++ "--")       

showBodyPart :: BodyPart -> ByteString
showBodyPart (BodyPart hs c) = 
    unlinesCRLF $ [BS.pack (n++": "++v) | (n,v) <- hs] ++ [BS.empty,c]


--
-- * Splitting into multipart parts.
--

-- | Split a multipart message into the multipart parts.
splitParts :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString 
           -> [ByteString]
splitParts b = spl . dropPreamble b
  where
  spl x = case splitAtBoundary b x of
            Nothing -> []
            Just (s1,d,s2) | isClose b d -> [s1]
                           | otherwise -> s1:spl s2

-- | Drop everything up to and including the first line starting 
--   with the boundary.
dropPreamble :: ByteString -- ^ The boundary, without the initial dashes
             -> ByteString 
             -> ByteString
dropPreamble b s | BS.null s = BS.empty
                 | isBoundary b s = dropLine s
                 | otherwise = dropPreamble b (dropLine s)

-- | Split a string at the first boundary line.
splitAtBoundary :: ByteString -- ^ The boundary, without the initial dashes
                -> ByteString -- ^ String to split.
                -> Maybe (ByteString,ByteString,ByteString)
                   -- ^ The part before the boundary, the boundary line,
                   --   and the part after the boundary line. The CRLF
                   --   before and the CRLF (if any) after the boundary line
                   --   are not included in any of the strings returned.
                   --   Returns 'Nothing' if there is no boundary.
splitAtBoundary b s = spl 0
  where
  spl i = case findCRLF (BS.drop i s) of
              Nothing -> Nothing
              Just (j,l) | isBoundary b s2 -> Just (s1,d,s3)
                         | otherwise -> spl (i+j+l)
                  where 
                  s1 = BS.take (i+j) s
                  s2 = BS.drop (i+j+l) s
                  (d,s3) = splitAtCRLF s2

-- | Check whether a string starts with two dashes followed by
--   the given boundary string.
isBoundary :: ByteString -- ^ The boundary, without the initial dashes
           -> ByteString
           -> Bool
isBoundary b s = startsWithDashes s && b `BS.isPrefixOf` BS.drop 2 s

-- | Check whether a string for which 'isBoundary' returns true
--   has two dashes after the boudary string.
isClose :: ByteString -- ^ The boundary, without the initial dashes
        -> ByteString 
        -> Bool
isClose b s = startsWithDashes (BS.drop (2+BS.length b) s)

-- | Checks whether a string starts with two dashes.
startsWithDashes :: ByteString -> Bool
startsWithDashes s = BS.pack "--" `BS.isPrefixOf` s


--
-- * RFC 2046 CRLF
--

crlf :: ByteString
crlf = BS.pack "\r\n"

unlinesCRLF :: [ByteString] -> ByteString
unlinesCRLF = BS.concat . intersperse crlf

-- | Drop everything up to and including the first CRLF.
dropLine :: ByteString -> ByteString
dropLine s = snd (splitAtCRLF s)

-- | Split a string at the first empty line. The CRLF (if any) before the
--   empty line is included in the first result. The CRLF after the
--   empty line is not included in the result.
--   If there is no empty line, the entire input is returned
--   as the first result.
splitAtEmptyLine :: ByteString -> (ByteString, ByteString)
splitAtEmptyLine s | startsWithCRLF s = (BS.empty, dropCRLF s)
                   | otherwise = spl 0
  where
  spl i = case findCRLF (BS.drop i s) of
              Nothing -> (s, BS.empty)
              Just (j,l) | startsWithCRLF s2 -> (s1, dropCRLF s2)
                         | otherwise -> spl (i+j+l)
                where (s1,s2) = BS.splitAt (i+j+l) s

-- | Split a string at the first CRLF. The CRLF is not included
--   in any of the returned strings.
--   If there is no CRLF, the entire input is returned
--   as the first string.
splitAtCRLF :: ByteString -- ^ String to split.
            -> (ByteString,ByteString)
splitAtCRLF s = case findCRLF s of
                  Nothing -> (s,BS.empty)
                  Just (i,l) -> (s1, BS.drop l s2)
                      where (s1,s2) = BS.splitAt i s

-- | Get the index and length of the first CRLF, if any.
findCRLF :: ByteString -- ^ String to split.
         -> Maybe (Int64,Int64)
findCRLF s = 
    case findCRorLF s of
              Nothing -> Nothing
              Just j | BS.null (BS.drop (j+1) s) -> Just (j,1)
              Just j -> case (BS.index s j, BS.index s (j+1)) of
                           ('\n','\r') -> Just (j,2)
                           ('\r','\n') -> Just (j,2)
                           _           -> Just (j,1)

findCRorLF :: ByteString -> Maybe Int64
findCRorLF s = BS.findIndex (\c -> c == '\n' || c == '\r') s

startsWithCRLF :: ByteString -> Bool
startsWithCRLF s = not (BS.null s) && (c == '\n' || c == '\r')
  where c = BS.index s 0

-- | Drop an initial CRLF, if any. If the string is empty, 
--   nothing is done. If the string does not start with CRLF,
--   the first character is dropped.
dropCRLF :: ByteString -> ByteString
dropCRLF s | BS.null s = BS.empty
           | BS.null (BS.drop 1 s) = BS.empty
           | c0 == '\n' && c1 == '\r' = BS.drop 2 s
           | c0 == '\r' && c1 == '\n' = BS.drop 2 s
           | otherwise = BS.drop 1 s
  where c0 = BS.index s 0
        c1 = BS.index s 1
