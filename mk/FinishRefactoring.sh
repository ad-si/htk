#! /bin/sh

echo "move Folders back"

#This function first move the folder, 
#then create a folder for the sources in 
#the folder, add the folder and move all
#.hs files.
MoveCreateAddCopy()
{
#change to lowerletter
mv $1 $2

cd $2

#Create Sourcefolder
mkdir $1
#svn add $1 

#Move all subfolder and .hs files into Sourcefolder
#for changes on the repository, please uncomment
#the svn lines and comment the two local lines (mv...)
mv */ $1
#svn move */ $1 
mv *.hs $1
#svn move *.hs $1

cd ..
}



MoveCreateAddCopy Util util
MoveCreateAddCopy Appl appl
MoveCreateAddCopy Database database
MoveCreateAddCopy UDrawGraph udrawgraph
MoveCreateAddCopy Emacs emacs/
MoveCreateAddCopy Events events/
MoveCreateAddCopy Graphs graphs/
MoveCreateAddCopy Htk htk/ 
MoveCreateAddCopy Imports  imports/
MoveCreateAddCopy MMiSS mmiss/
MoveCreateAddCopy Posixutil posixutil/
MoveCreateAddCopy Reactor reactor/
MoveCreateAddCopy Server server/
MoveCreateAddCopy Simpledb simpledb/
MoveCreateAddCopy Types types/




