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
sed "s/import Debug(debug)/import Util.Debug(debug)/" -i */*/*.hs
sed "s/import ExtendedPrelude(HasMapIO(..))/import Util.ExtendedPrelude(HasMapIO(..))/" -i */*.hs
sed "s/import BinaryInstances()/import Util.BinaryInstances()/" -i */*.hs
sed "s/import qualified DeprecatedFiniteMap/import qualified Util.DeprecatedFiniteMap as DeprecatedFiniteMap/" -i */*.hs
sed "s/import ExtendedPrelude(newFallOut,mkBreakFn)/import Util.ExtendedPrelude(newFallOut,mkBreakFn)/" -i */*.hs
sed "s/import Object(ObjectID)/import Util.Object(ObjectID)/" -i */*.hs
sed "s/import Computation (done)/import Util.Computation (done)/" -i */*.hs
sed "s/import BaseClasses(Widget)/import Htk.Kernel.BaseClasses(Widget)/" -i */*/*.hs
sed "s/import qualified ExtendedPrelude(simpleSplit)/import qualified Util.ExtendedPrelude as ExtendedPrelude/" -i */*/*.hs
sed "s/import Geometry(Distance)/import Htk.Kernel.Geometry(Distance)/" -i */*/*.hs
sed "s/import ExtendedPrelude(simpleSplit)/import Util.ExtendedPrelude(simpleSplit)/" -i */*/*.hs
sed "s/import Object(ObjectID(..))/import Util.Object(ObjectID(..))/" -i */*/*.hs
sed "s/import Colour(toColour)/import Htk.Kernel.Colour(toColour)/" -i */*/*.hs
sed "s/import BaseClasses (Widget)/import Htk.Kernel.BaseClasses (Widget)/" -i */*/*.hs
sed "s/import Selection hiding (HasIndex, getBaseIndex)/import Htk.Components.Selection as Selection/" -i */*/*.hs
sed "s/import Selection hiding (HasIndex,getBaseIndex)/import Htk.Components.Selection as Selection/" -i */*/*.hs
sed "s/import CanvasItemAux (canvasitemMethods)/import Htk.Canvasitems.CanvasItemAux (canvasitemMethods)/" -i */*/*.hs
sed "s/import GUIObjectName (CanvasTagOrID (..))/import Htk.Kernel.GUIObjectName (CanvasTagOrID (..))/" -i */*/*.hs
sed "s/import Geometry (Position,Distance)/import Htk.Kernel.Geometry (Position,Distance)/" -i */*/*.hs
sed "s/import ExtendedPrelude (newFallOut,mkBreakFn)/import Util.ExtendedPrelude (newFallOut,mkBreakFn)/" -i */*/*.hs
sed "s/import DialogWin (createWarningWin,createConfirmWin)/import Htk.Toolkit.DialogWin (createWarningWin,createConfirmWin)/" -i */*/*.hs
sed "s/import Configuration(HasText(..))/import Htk.Kernel.Configuration(HasText(..))/" -i */*/*.hs
sed "s/import Packer(Container)/import Htk.Kernel.Packer(Container)/" -i */*/*.hs
sed "s/import Core(HasCommand(..))/import Htk.Kernel.Core(HasCommand(..))/" -i */*/*.hs
sed "s/import Computation (try)/import Util.Computation (try)/" -i */*/*.hs
sed "s/import Examples(watch)/Events.Examples as Examples/" -i */*/*.hs
sed "s/import BinaryAll(HasBinary(..),mapWrite,mapRead)/import Util.BinaryAll(HasBinary(..),mapWrite,mapRead)/" -i */*/*.hs
sed "s/import GUIObject(toGUIObject)/import Htk.Kernel.GUIObject(toGUIObject)/" -i */*/*.hs
sed "s/import Core(GUIObject(..))/import Htk.Kernel.Core(GUIObject(..))/" -i */*/*.hs

rm temp
