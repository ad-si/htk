#! /bin/sh

echo "delete old folders"

rm -r -f -f Appl
rm -r -f Database
rm -r -f UDrawGraph
rm -r -f Emacs
rm -r -f Events
rm -r -f Graphs
rm -r -f Htk 
rm -r -f Imports
rm -r -f MMiSS
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
mv graphs/ Graphs
mv htk/ Htk 
mv imports/ Imports
mv mmiss/ MMiSS
mv posixutil/ Posixutil
mv reactor/ Reactor
mv server/ Server
mv simpledb/  Simpledb
mv types/ Types
mv util/ Util

#Util und Graphs Test groß und umbennen (nicht main und test).
#subfolders to uppercase
#DaVinci in files remove
#
cd Appl
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
