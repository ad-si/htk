{- --------------------------------------------------------------------
 -
 - file browse example (as an example for the treelist module)
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - -------------------------------------------------------------------- -}

module Main (main) where

import HTk
import TreeList
import Directory
import System

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _ = False

getMatchedFiles :: [FilePath] -> FilePath -> IO [FilePath]
getMatchedFiles fs abs = getMatchedFiles' fs [] abs
  where getMatchedFiles' :: [FilePath] -> [FilePath] -> FilePath ->
                            IO [FilePath]
        getMatchedFiles' (f : fs) dirs abs =
          do
            p' <- try (getPermissions (abs ++ f))
            case p' of
              Right p -> if f == "." || f == ".." || not(searchable p) ||
                            hidden f then
                           getMatchedFiles' fs dirs abs
                         else getMatchedFiles' fs (f : dirs) abs
              Left _ -> getMatchedFiles' fs (f : dirs) abs
        getMatchedFiles' _ dirs _ = return dirs

node :: FilePath -> FilePath -> IO Bool
node abs fp =
  do
    c' <- try (getDirectoryContents (abs ++ fp))
    case c' of
      Right c ->
        let containsFolder (f : fs) =
              do
                p' <- try (getPermissions (abs ++ fp ++ "/" ++ f))
                case p' of
                  Right p ->
                    if (searchable p && not(hidden f)) then return True
                    else containsFolder fs
                  _ -> containsFolder fs
            containsFolder _ = return False
        in containsFolder c
      _ -> return False

toTreeListObjects :: String -> [FilePath] -> IO [TreeListObject String]
toTreeListObjects path (f : fs) =
  do
    p <- getPermissions (path ++ f)
    acc <- system ("access -rx " ++ path)
    isnode <- if acc == ExitSuccess then
                do
                  b <- node path f
                  return (if b then Node else Leaf)
              else return Leaf
    let obj = newTreeListObject (path ++ f ++ "/") f isnode
    objs <- toTreeListObjects path fs
    return (obj : objs)
toTreeListObjects _ _ = return []

cfun :: ChildrenFun String
cfun obj =
  do
    let val = getTreeListObjectValue obj
    dcontents <- getDirectoryContents val
    matched_files <- getMatchedFiles dcontents val
    objs <- toTreeListObjects val matched_files
    return objs

ifun :: ImageFun String
ifun _ = folderImg

main :: IO ()
main =
  do
    main <- initHTk [text "file browse example"]
    treelist <- newTreeList main cfun ifun
                            (newTreeListObject "/" "/" Node)
                            [background "white", size (cm 9, cm 10)]
    pack treelist []

    quit <- newButton main [text "Quit", width 15] :: IO (Button String)
    pack quit [Side AtBottom, PadX 10, PadY 5]

    clickedquit <- clicked quit
    spawnEvent (forever (clickedquit >> always (destroy main)))

    (htk_destr, _) <- bindSimple main Destroy
    sync htk_destr
    destroy main    -- TD: Wish bleibt manchmal liegen. So gehts? Bug!

folderImg = newImage NONE [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ
1UV+qjB659uWkBlj9tIBw873BQA7
"]
