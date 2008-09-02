#! /bin/sh

echo "Creating datahierarchy"

change()
{
	#path of the file 
	Path=`sed -e "s/a/a/" temp`
	#folder with modulehierarchy	
	Name_hierarchic=`sed -e "s/\//./g" -e "s/.hs.*//" temp`
	#name of the file
	Name=`sed -e "s/.hs//" -e "s/.*\///" temp`
	#defining folders which dont have to be changed

		echo "Changing module "$Name "to "$Name_hierarchic "in "$Path
		#replacing the name of the module
		sed "s/module $Name/module $Name_hierarchic/" -i $Path
		#replacing the name of the module in the other files
		sed "s/import $Name$/import $Name_hierarchic as $Name/" -i */*.hs
		#sed "s/import $Name[ ]/import $Name_hierarchic as $Name/" -i */*.hs
		sed "s/import $Name$/import $Name_hierarchic as $Name/" -i */*/*.hs
		#sed "s/import $Name[ ]/import $Name_hierarchic as $Name/" -i */*/*.hs
		sed "s/import $Name$/import $Name_hierarchic as $Name/" -i */*/*/*.hs
		#sed "s/import $Name[ ]/import $Name_hierarchic as $Name/" -i */*/*/*.hs
		#sed "s/import qualified  $Name$/import qualified $Name_hierarchic as $Name/" -i */*.hs
		#sed "s/import qualified  $Name[ ]/import qualified $Name_hierarchic as $Name/" -i */*.hs
		#sed "s/import qualified  $Name$/import qualified $Name_hierarchic as $Name/" -i */*/*.hs
		#sed "s/import qualified  $Name[ ]/import qualified $Name_hierarchic as $Name/" -i */*/*.hs	
		#sed "s/import qualified  $Name$/import qualified $Name_hierarchic as $Name/" -i */*/*/*.hs
		#sed "s/import qualified  $Name[ ]/import qualified $Name_hierarchic as $Name/" -i */*/*/*.hs	
}

beginChange()
{
#Loop for all Haskell-Files in the Folder
for file in $Folder
do
	#temporary saving of the file 
	echo $file > temp
	Needchange=`sed -e "s/.*test.*/not/" -e "s/.*HaXml.*/not/" -e "s/.*includes.*/not/" -e "s/.*mk.*/not/" temp`
	if [ $Needchange != "not" ]
	then
		#beginning the change
		change
	else
		echo $file "has not to be changed"
	fi
done
}

#first 3 levels will be changed

#Level 1
Folder="*/*.hs"
beginChange
#Level 2
Folder="*/*/*.hs"
beginChange
#Level 3
Folder="*/*/*/*.hs"
beginChange

#Exceptions
echo "Working on some Exceptions"
sed "s/import Computation(done)/import Util.Computation(done)/" -i */*.hs
sed "s/import Debug(debug)/import Util.Debug(debug)/" -i */*.hs
sed "s/import ExtendedPrelude(HasMapIO(..))/import Util.ExtendedPrelude(HasMapIO(..))/" -i */*.hs
sed "s/import BinaryInstances()/import Util.BinaryInstances()/" -i */*.hs
sed "s/import qualified DeprecatedFiniteMap/import qualified Util.DeprecatedFiniteMap as DeprecatedFiniteMap/" -i */*.hs
sed "s/import ExtendedPrelude(newFallOut,mkBreakFn)/import Util.ExtendedPrelude(newFallOut,mkBreakFn)/" -i */*.hs
sed "s/import Object(ObjectID)/import Util.Object(ObjectID)/" -i */*.hs
sed "s/import Computation (done)/import Util.Computation (done)/" -i */*.hs


rm temp
