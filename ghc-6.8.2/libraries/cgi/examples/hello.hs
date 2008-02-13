import Network.CGI (CGI, CGIResult, output, runCGI, handleErrors)

cgiMain :: CGI CGIResult
cgiMain = output "Hello World!"

main :: IO ()
main = runCGI (handleErrors cgiMain)
