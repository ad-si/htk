{- FileNames contain facilities for manipulating filenames
   in a hopefully OS-independent manner.  -}
module FileNames(
   fileSep, -- :: Char
            -- file separator
   trimDir, -- :: String -> String
            -- trim file separator from end of name if there.
            -- (intended for directories)
   splitName,
            -- :: String -> Maybe(String,String)
            -- If path name contains both a directory and a file name
            -- portion, return them.  None of the names should end with
            -- the file separator. 
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
   
            
   ) where

import CompileFlags

$(
   if isWindows
      then
         [d|
            fileSep :: Char
            fileSep = '\\'
         |]
      else
         [d|
            fileSep :: Char
            fileSep = '/'
         |]
   )

trimDir :: String -> String
trimDir [] = []
trimDir (name@[c]) 
   | c==fileSep = []
   | True = name
trimDir (first:rest) = first:trimDir rest

splitName :: String -> Maybe(String,String)
splitName [] = Nothing
splitName (first:remainder) =
   case splitName remainder of
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


unbreakName :: [String] -> String
unbreakName [] = ""
unbreakName parts = foldr1 combineNames parts


splitExtension :: String -> Maybe (String,String)
splitExtension str = case splitExtension0 str of
      Just (ne @ (name,ext)) | not (null name),not (null ext) -> Just ne
      _ -> Nothing
   where
      splitExtension0 [] = Nothing
      splitExtension0 (c:cs) = case splitExtension0 cs of
         Just (name0,ext) -> Just (c:name0,ext)
         Nothing -> if c == '.' then Just ("",cs) else Nothing

unsplitExtension :: String -> String -> String
unsplitExtension name ext = name ++ "." ++ ext