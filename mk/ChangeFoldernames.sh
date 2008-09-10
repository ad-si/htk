#! /bin/sh

createDir ()
{
echo creating $1/$2 and copying sources
cd $1
mkdir -p $2
cp *.hs $2
cd ..
}

mkdir -p appl/Appl
createDir davinci UDrawGraph
createDir emacs Emacs
createDir events Events
createDir graphs Graphs
mkdir -p htk/Htk
createDir imports Imports
createDir mmiss MMiSS
createDir posixutil Posixutil
createDir reactor Reactor
createDir server Server
createDir simpledb  Simpledb
createDir types Types
createDir util Util

createSubDir ()
{
echo creating $1/$3 and copying sources
rm -rf $1/$3
cp -r $2 $1/$3
}

cd htk
createSubDir Htk canvasitems Canvasitems
createSubDir Htk components Components
createSubDir Htk containers Containers
createSubDir Htk devices Devices
createSubDir Htk kernel Kernel
createSubDir Htk menuitems Menuitems
createSubDir Htk textitems Textitems
createSubDir Htk tix Tix
createSubDir Htk toolkit Toolkit
createSubDir Htk toplevel Toplevel
createSubDir Htk widgets Widgets
cd ..

cd appl
createSubDir Appl ontologytool Ontologytool
cd ..

cd mmiss
createSubDir MMiSS api Api
createSubDir MMiSS parser Parser
cd ..

renameHierFile ()
{
echo renaming $1 files
for file in $1*.hs
do
   echo $file > temp
   newfile=`sed -e "s/$1//" temp`
   cp -f $file $newfile
   rm -f $file temp
done
}

cd davinci/UDrawGraph
renameHierFile DaVinci
cd ../..

cd mmiss/MMiSS
renameHierFile MMiSS
cd Api
renameHierFile MMiSS
cd ../../..

mk/ReplaceModuleNames */[A-Z]*/*.hs
mk/ReplaceModuleNames */[A-Z]*/[A-Z]*/*.hs
