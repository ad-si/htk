-- #####################################################################
--
-- MODULE		: FileActions
-- AUTHOR		: Joel Wright
-- DATE			: 2000
-- VERSION		: Alpha
-- DESCRIPTION		: Implements file operations 
--			  (copy, move, delete etc ...)
--
-- #####################################################################

module FileActions (

	selectIcon,
	makeNameForIcon
	
		   ) where
		   
import Directory


-- #####################################################################
-- The file actions
-- #####################################################################


-- type FilePath = String

-- moveFile :: FilePath -> FilePath -> IO()

-- copyFile :: FilePath -> FilePath -> IO()

makeNameForIcon :: String -> IO String
makeNameForIcon filename = do
	if ((length(filename)) > 15) then
		(return ((take 12 filename)++"..."))
		else
		(return filename)

selectIcon :: String -> IO String
selectIcon file = case reverse(file) of
	'f':'i':'g':'.':_ -> return "./icons/pictureIcon.gif"
	'p':'m':'b':'.':_ -> return "./icons/pictureIcon.gif"
	'g':'e':'p':'j':'.':_ -> return "./icons/pictureIcon.gif"
	'g':'p':'j':'.':_ -> return "./icons/pictureIcon.gif"
	'f':'i':'t':'.':_ -> return "./icons/pictureIcons.gif"
	'c':'.':_ -> return "./icons/cIcon.gif"
	'c':'c':'.':_ -> return "./icons/ccIcon.gif"
	'h':'.':_ -> return "./icons/hIcon.gif"
	's':'h':'.':_ -> return "./icons/haskellIcons.gif"
	'o':'.':_ -> return "./icons/objectIcon.gif"
	'i':'h':'.':_ -> return "./icons/hiIcon.gif"
	'l':'m':'t':'h':'.':_ -> return "./icons/htmlIcon.gif"
	'm':'t':'h':'.':_ -> return "./icons/htmlIcon.gif"
	's':'p':'.':_ -> return "./icons/psIcon.gif"
	's':'p':'e':'.':_ -> return "./icons/epsIcon.gif"
	'v':'o':'m':'.':_ -> return "./icons/movIcon.gif"
	'i':'v':'a':'.':_ -> return "./icons/aviIcon.gif"
	'g':'e':'p':'m':'.':_ -> return "./icons/mpgIcon.gif"
	'g':'p':'m':'.':_ -> return "./icons/mpgIcon.gif"
	't':'x':'t':'.':_ -> return "./icons/textIcon.gif"
	_ -> return "./icons/defaultIcon.gif"


