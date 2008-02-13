#!/bin/sh

sed -i.orig -e 's/^Name: cgi/Name: cgi-compat/' -e 's/Network\.CGI,/Network.NewCGI,/' -e 's/Build-depends:\(.*\)/Build-depends:\1, fps/' cgi.cabal
sed -e 's/^module Network\.CGI/module Network.NewCGI/' < Network/CGI.hs > Network/NewCGI.hs
