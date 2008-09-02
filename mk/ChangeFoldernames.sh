#! /bin/bash -x

echo "delete old folders"

rm -r -f appl/Appl
rm -r -f database/Database
rm -r -f davinci/UDrawGraph
rm -r -f emacs/Emacs
rm -r -f events/Events
rm -r -f graphs/Graphs
rm -r -f htk/Htk
rm -r -f imports/Imports
rm -r -f mmiss/MMiSS
rm -r -f posixutil/Posixutil
rm -r -f reactor/Reactor
rm -r -f server/Server
rm -r -f simpledb/Simpledb
rm -r -f types/Types
rm -r -f util/Util

echo "rename folders"

mkDirAndCopy ()
{
mkdir $1; cp *.hs $1
}

(cd  appl; mkDirAndCopy Appl)
(cd  database; mkDirAndCopy Database)
(cd  davinci; mkDirAndCopy UDrawGraph)
(cd  emacs; mkDirAndCopy Emacs)
(cd  events; mkDirAndCopy Events)
(cd  graphs; mkDirAndCopy Graphs)
(cd  htk; mkDirAndCopy Htk)
(cd  imports; mkDirAndCopy Imports)
(cd  mmiss; mkDirAndCopy MMiSS)
(cd  posixutil; mkDirAndCopy Posixutil)
(cd  reactor; mkDirAndCopy Reactor)
(cd  server; mkDirAndCopy Server)
(cd  simpledb; mkDirAndCopy  Simpledb)
(cd  types; mkDirAndCopy Types)
(cd  util; mkDirAndCopy Util)

#Util und Graphs Test groß und umbennen (nicht main und test).
#subfolders to uppercase
#DaVinci in files remove
#
cd appl/Appl
mv ontologytool/ Ontologytool/
mv uDrawOntology/ UDrawOntology/
	cd UDrawOntology
	mv doc/ Doc/
	mv resource/ Resource/
	mv samples/ Samples/
	cd ..
cd ..

cd Database
mv icons/ Icons/
mv images/ Images/
cd ..

#mv Emacs/test/
#mv Events/test/

cd Graphs
mv test/ Test/
cd ..

cd Htk
mv canvasitems/ Canvasitems/
mv components/ Components/
mv containers/ Containers/
mv devices/ Devices/
mv examples/ Examples/
mv htk/ Htk/
mv kernel/ Kernel/
mv menuitems/ Menuitems/
mv packer/ Packer/
mv resources/ Resources/
#mv Htk/test/
mv textitems/ Textitems/
mv tix/ Tix/
mv toolkit/ Toolkit/
mv toplevel/ Toplevel/
mv widgets/ Widgets/
	cd Examples
	mv canvas/ Canvas/
	mv gengui/ Gengui/
	mv images/ Images/
	mv intro/ Intro/
	mv simple/ Simple/
	mv tix/ Tix/
	mv toolkit/ Toolkit/
	cd ..
cd ..

#mv Imports/test/

cd MMiSS
mv api/ Api/
mv checker/ Checker/
mv coupling/ Coupling/
mv mmisslatex/ MMiSSlatex/
mv parser/ Parser/
mv scripts/ Scripts/
	cd Api
	mv dummyserver/ Dummyserver/
	cd ..
cd ..

#mv MMiss/test/ MMiss/test/
#mv Posixutil/test/
#mv Server/test/
#mv share/doc/ share/doc/
#mv Simpledb/test/
#mv Types/test/
#mv UDrawGraph/test/
cd Util
mv test/ Test/
cd ..
#mv Appl/ontologytool/test/


#mv MMiss/api/test/ MMiss/api/test/
#mv MMiSS/Checker/src/ MMiSS/Checker/src/
#mv MMiss/coupling/test/
#mv MMiss/parser/test/
#mv MMiss/test/files/
#mv share/doc/uni-util-1.1/ share/doc/uni-util-1.1/
cd MMiSS
for file in *.hs
do
	echo $file > temp
	oldmodule_name=`sed -e "s/.hs.*//" temp`
	newfilename=`sed -e "s/MMiSS//" temp`
	newmodule_name=`sed -e "s/.hs.*//" temp`
	#temporary saving of the file
	mv $file $newfilename
	sed "s/$oldmodule_name/$newmodule_name/" -i */*/*.hs
	sed "s/$oldmodule_name/$newmodule_name/" -i */*.hs
done

cd Api
for file in *.hs
do
	echo $file > temp
	newfilename=`sed -e "s/MMiSS//" temp`
	#temporary saving of the file
	mv $file $newfilename
done

cd ..
cd ..

sed "s/module MMiSS/module /" -i */*/*.hs
sed "s/module MMiSS/module /" -i */*.hs
sed "s/import MMiSS/import /" -i */*/*.hs
sed "s/import MMiSS/import /" -i */*.hs

rm temp
