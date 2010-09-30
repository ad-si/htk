{-# LANGUAGE CPP #-}
-- | FileNames contain facilities for manipulating filenames
-- in a hopefully OS-independent manner.
module Util.FileNames(
   fileSep, -- :: Char
            -- file separator
   topDir,  -- :: String
            -- what we call the top directory.
   thisDir,  -- :: String
            -- what we call the current directory.
   trimDir, -- :: String -> String
            -- trim file separator from end of name if there.
            -- (intended for directories)
   splitName,
            -- :: String -> (String,String)
            -- Returns the directory and file part of a name.
   combineNames,
            -- :: String -> String -> String
            -- combines a directory and file name.
   breakName,
            -- :: String -> [String]
            -- breakName splits local file name completely into
            -- a sequence of file names, with the top directory
            -- first.  (If the first character is the file separator
            -- the first list element is the empty string.)
   unbreakName,
            -- :: [String] -> String
            -- unbreakName inverts breakName

   splitExtension,
            -- :: String -> Maybe (String,String)
            -- Remove the (last) extension part from a file name, returning
            -- the two parts.  For example "foo.bar" should go to (foo,bar).
   unsplitExtension,
            -- :: String -> String -> String
            -- reverse unsplitExtension.

   recordSep,
            -- :: String
            -- separator for between records.

   ) where

#ifdef WINDOWS
fileSep = '\\'
recordSep = "\r\n"
#else
fileSep = '/'
recordSep = "\n"
#endif

fileSep :: Char
recordSep :: String

topDir :: String
topDir = [fileSep]

thisDir :: String
thisDir = "."

trimDir :: String -> String
trimDir [] = []
trimDir (name@[c])
   | c==fileSep = []
   | True = name
trimDir (first:rest) = first:trimDir rest

splitName :: String -> (String,String)
splitName filePath0 =
   let
      filePath1 = trimDir filePath0
   in
      case splitName1 filePath1 of
         Nothing -> (thisDir,filePath1)
         Just ("",filePath2) -> (topDir,filePath2)
         Just simple -> simple

splitName1 :: String -> Maybe(String,String)
splitName1 [] = Nothing
splitName1 (first:remainder) =
   case splitName1 remainder of
      Nothing
         | first == fileSep -> Just([],remainder)
         | True -> Nothing
      Just (dir,name) -> Just (first:dir,name)

combineNames :: String -> String -> String
combineNames dir file = dir ++ (fileSep:file)

breakName :: String -> [String]
breakName [] = [[]]
breakName (first : rest)
   | (first == fileSep) = "":breakName rest
   | True =
      case breakName rest of
         firstName : restNames -> (first:firstName) : restNames
         [] -> error "breakName"

unbreakName :: [String] -> String
unbreakName [] = ""
unbreakName parts = foldr1 combineNames parts


splitExtension :: String -> Maybe (String,String)
splitExtension str = case splitExtension0 str of
      Just (ne @ (name,ext)) | not (null name) && not (null ext) -> Just ne
      _ -> Nothing
   where
      splitExtension0 [] = Nothing
      splitExtension0 (c:cs) = case splitExtension0 cs of
         Just (name0,ext) -> Just (c:name0,ext)
         Nothing -> if c == '.' then Just ("",cs) else Nothing

unsplitExtension :: String -> String -> String
unsplitExtension name ext = name ++ "." ++ ext
