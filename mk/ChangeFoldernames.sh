#! /bin/sh

echo "delete old folders"

rm -r -f -f Appl
rm -r -f Database
rm -r -f UDrawGraph
rm -r -f Emacs
rm -r -f Events
rm -r -f Graph
rm -r -f Htk 
rm -r -f Imports
rm -r -f MMiss
rm -r -f Posixutil
rm -r -f Reactor
rm -r -f Server
rm -r -f Simpledb
rm -r -f Types
rm -r -f Util

echo "rename folders"

mv appl/ Appl
mv database/ Database
mv davinci/ UDrawGraph
mv emacs/ Emacs
mv events/ Events
mv graphs/ Graph
mv htk/ Htk 
mv imports/ Imports
mv mmiss/ MMiss
mv posixutil/ Posixutil
mv reactor/ Reactor
mv server/ Server
mv simpledb/  Simpledb
mv types/ Types
mv util/ Util





