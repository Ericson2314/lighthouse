-- Redirect to the URL given by the url parameter.

import Network.CGI (MonadCGI, CGIResult, runCGI, getInput, output, redirect)

redirectToURL :: MonadCGI m => m CGIResult
redirectToURL = 
    getInput "url" >>= maybe (output "url parameter not set!\n") redirect

main =
   runCGI redirectToURL
