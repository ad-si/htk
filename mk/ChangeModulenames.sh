#! /bin/sh

echo "Datenstruktur erstellen"

change()
{
#Schleife für alle Dateien
for file in */*.hs
do
	#Temporäres Zwischenspeichern der Datei
	echo $file > temp
	#Ordner der Datei 
	Ordner=`sed "s/\/[_-Z]*.hs//" temp`
	#Name der Datei
	Name=`sed -e "s/.hs//" -e "s/[_-Z]*\///" temp`
	#Ordner definieren, die nicht geändert werden sollen
	if [ $Ordner != "mk" -a  $Ordner != "includes" -a $Ordner != "test" ]
	then
		echo "Bearbeiten von "$Name "in "$Ordner 
		#Ersetzen des Modulnamens
		sed "s/module $Name/module $Ordner.$Name/" -i $Ordner/$Name.hs
		#Ersetzen des neuen Modulnamens in den anderen Programmen
		sed "s/import $Name/import $Ordner.$Name/" -i */*.hs
		sed "s/import qualified  $Name/import qualified $Ordner.$Name/" -i */*.hs
	else
		echo $Ordner"/"$Name "wird nicht bearbeitet"
	fi
done
#Löschen der temporären Datei
rm temp
}

#Starten des Ersetzens
change

