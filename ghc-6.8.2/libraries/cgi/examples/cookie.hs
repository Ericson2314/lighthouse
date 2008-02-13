import Network.CGI
   ( CGI, CGIResult, runCGI, output, setCookie, newCookie, getCookie
   )
import Text.XHtml
   ( Html, h1, p, header, body, (+++), thetitle, (<<), renderHtml
   )

setCounterCookie :: Int -> CGI ()
setCounterCookie n = setCookie (newCookie "mycookie" (show n))

firstTime :: CGI [Html]
firstTime = do setCounterCookie 1
               return [h1 << "Welcome!"]

returnVisitor :: Int -> CGI [Html]
returnVisitor c = 
    do setCounterCookie (c + 1)
       return [h1 << "Welcome back!",
               p << ("I have seen you " ++ show c ++ " times before.")]

cgiMain :: CGI CGIResult
cgiMain = do mc <- getCookie "mycookie"
             h <- maybe firstTime (returnVisitor . read) mc
             output $ renderHtml $ header << thetitle << "Cookie example" 
                                    +++ body << h

main :: IO ()
main = runCGI cgiMain
