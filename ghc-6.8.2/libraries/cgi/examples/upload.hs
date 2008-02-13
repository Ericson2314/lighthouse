-- Accepts file uploads and saves the files in the given directory.
-- WARNING: this script is a SECURITY RISK and only for 
-- demo purposes. Do not put it on a public web server.

import Control.Monad (liftM)
import Data.Maybe (fromJust)

import qualified Data.ByteString.Lazy as BS (writeFile)
import Network.CGI (runCGI, getInputFPS, getInputFilename, output, liftIO)
import Text.XHtml
   ( paragraph, (!), href, (+++), form, method, enctype, afile, submit
   , renderHtml, header, thetitle, body, (<<), anchor
   )

dir = "../upload"

saveFile n =
    do cont <- liftM fromJust $ getInputFPS "file"
       let p = dir ++ "/" ++ basename n
       liftIO $ BS.writeFile p cont
       return $ paragraph << ("Saved as " +++ anchor ! [href p] << p +++ ".")

fileForm = form ! [method "post", enctype "multipart/form-data"]
             << [afile "file", submit "" "Upload"]


basename = reverse . takeWhile (`notElem` "/\\") . reverse

cgiMain = 
    do mn <- getInputFilename "file"
       h <- maybe (return fileForm) saveFile mn
       output $ renderHtml $ header << thetitle << "Upload example" 
                               +++ body << h

main = runCGI cgiMain
