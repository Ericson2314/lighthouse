-- Prints the values of all CGI variables and inputs.

import Network.CGI
   ( CGI, CGIResult, runCGI, output, setHeader, getMultiInput
   , serverName, serverPort, getInputFilename, requestMethod, pathInfo
   , pathTranslated, scriptName, remoteHost, remoteAddr, remoteUser
   , queryString, authType, requestContentType, requestContentLength
   , requestHeader, progURI, queryURI, requestURI, getVars, getInputNames
   )

import Control.Monad (liftM)
import Data.List (intersperse)


prInput :: String -> CGI String
prInput i = 
    do
    vs <- getMultiInput i
    let v = concat $ intersperse "," $ map show vs
    f <- getInputFilename i
    return $ case f of
           Just n -> i ++ ": File\nfilename=" ++ n
                     ++ "\ncontents=" ++ v
           Nothing -> i ++ ": " ++ v


envFuns = sequence 
       [
        f "serverName"           serverName,
        f "serverPort"           (liftM show serverPort),
        f "requestMethod"        requestMethod,
        f "pathInfo"             pathInfo,
        f "pathTranslated"       pathTranslated,
        f "scriptName"           scriptName,
        f "queryString"          queryString,
        f "remoteHost"           remoteHost,
        f "remoteAddr"           remoteAddr,
        f "authType"             authType,
        f "remoteUser"           remoteUser,
        f "requestContentType"   requestContentType ,
        f "requestContentLength" requestContentLength,
        f "requestHeader \"User-Agent\"" (requestHeader "User-Agent"),
        f "progURI"              progURI,
        f "queryURI"             queryURI,
        f "requestURI"           requestURI
       ]
  where f n = liftM (((,) n) . show)


prVars vs = unlines [k ++ ": " ++ x | (k,x) <- vs ]

cgiMain :: CGI CGIResult
cgiMain = do fs <- envFuns
             vs <- getVars
             is <- getInputNames
             i <- mapM prInput is
             setHeader "Content-type" "text/plain"
             output ("Environment:\n" ++ prVars fs
                     ++ "\nCGI Environment Variables:\n" ++ prVars vs
                     ++ "\nInputs:\n" ++ unlines i)

main = runCGI cgiMain
