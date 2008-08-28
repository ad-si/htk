#! /bin/sh

echo "delete old folders"

rm -r Appl
rm -r Database
rm -r UDrawGraph
rm -r Emacs
rm -r Events
rm -r Graph
rm -r Htk 
rm -r Imports
rm -r MMiss
rm -r Posixutil
rm -r Reactor
rm -r Server
rm -r Simpledb
rm -r Types
rm -r Util

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





