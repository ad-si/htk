#! /bin/sh

createDir ()
{
echo creating $1/$2 and copying sources
cd $1
mkdir -p $2
cp -f *.hs* $2
cd ..
}

mkdir -p appl/Appl
createDir davinci UDrawGraph
createDir emacs Emacs
createDir events Events
createDir graphs Graphs
mkdir -p htk/HTk
createDir imports Imports
createDir mmiss MMiSS
createDir posixutil Posixutil
createDir reactor Reactor
createDir server Server
createDir simpledb SimpleDB
createDir types Types
createDir util Util

createSubDir ()
{
echo creating $1/$3 and copying sources
mkdir -p $1/$3
cp -f $2/*.hs* $1/$3
}

cd htk
createSubDir HTk canvasitems Canvasitems
createSubDir HTk components Components
createSubDir HTk containers Containers
createSubDir HTk devices Devices
createSubDir HTk kernel Kernel
createSubDir HTk menuitems Menuitems
createSubDir HTk textitems Textitems
createSubDir HTk tix Tix
createSubDir HTk toolkit Toolkit
createSubDir HTk toplevel Toplevel
createSubDir HTk widgets Widgets
cd ..

cd appl
createSubDir Appl ontologytool Ontologytool
cd ..

cd mmiss
cp -f ../HaXml/XmlExtras.hs MMiSS
createSubDir MMiSS parser LaTeX
createSubDir MMiSS api API
cd ..

renameHierFile ()
{
echo renaming $1 files
if [ -f $1.hs ]
   then cp -f $1.hs Interface.hs
fi
for file in $1*.hs*
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

cd simpledb/SimpleDB
renameHierFile SimpleDB
cd ../..

cd emacs/Emacs
renameHierFile Emacs
cd ../..

cd mmiss/MMiSS
renameHierFile MMiSS
cd LaTeX
renameHierFile LaTeX
cd ../API
renameHierFile MMiSSAPI
renameHierFile MMiSS
cd ../../..

mk/ReplaceModuleNames */[A-Z]*/*.hs*
mk/ReplaceModuleNames */[A-Z]*/[A-Z]*/*.hs
