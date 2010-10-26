
module Main (main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.TreeList
import Directory
import System
import HTk.Toolkit.Name
import Control.Concurrent(threadDelay)
import Control.Exception

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _ = False

data FileObject = FileObject String String (IO Image)

instance Eq FileObject where
  FileObject path1 _ _ == FileObject path2 _ _ = path1 == path2

instance CItem FileObject where
  getName (FileObject _ nm _) = return (createName nm)
  getIcon (FileObject _ _ img) = img

getMatchedFiles :: [FilePath] -> FilePath -> IO [FilePath]
getMatchedFiles fs abs = getMatchedFiles' fs [] abs
  where getMatchedFiles' :: [FilePath] -> [FilePath] -> FilePath ->
                            IO [FilePath]
        getMatchedFiles' (f : fs) dirs abs =
          do
            p' <- try (getPermissions (abs ++ f))
            case p' of
              Right p -> do
                           b' <- try (return (not (searchable p)))
                           case b' of
                             Right b ->
                               if f == "." || f == ".." || b ||
                               hidden f then
                                 getMatchedFiles' fs dirs abs
                               else getMatchedFiles' fs (f : dirs) abs
                             Left (_ :: SomeException) ->
                                 getMatchedFiles' fs (f : dirs) abs
              Left (_ :: SomeException) ->
                  getMatchedFiles' fs (f : dirs) abs
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
                  Left (_ :: SomeException) -> containsFolder fs
            containsFolder _ = return False
        in containsFolder c
      Left (_ :: SomeException) -> return False

toTreeListObjects :: String -> [FilePath] ->
                     IO [TreeListObject FileObject]
toTreeListObjects path (f : fs) =
  do
    acc <- system ("access -rx " ++ path)
    isnode <- if acc == ExitSuccess then
                do
                  b <- node path f
                  return (if b then Node else Leaf)
              else return Leaf
    let obj = newTreeListObject (FileObject (path ++ f ++ "/") f
                                            folderImg) isnode
    objs <- toTreeListObjects path fs
    return (obj : objs)
toTreeListObjects _ _ = return []

cfun :: ChildrenFun FileObject
cfun obj =
  do
    let (FileObject path _ _) = getTreeListObjectValue obj
    dcontents <- getDirectoryContents path
    matched_files <- getMatchedFiles dcontents path
    objs <- toTreeListObjects path matched_files
    return objs

main :: IO ()
main =
  do
    main <- initHTk [text "treelist example", size (cm 9, cm 10)]
    tl <- newTreeList main cfun
            [newTreeListObject (FileObject "/" "/" folderImg) Node]
            [background "white"]
    pack tl [Fill Both, Expand On]

    (tlev, _) <- bindTreeListEv tl

    quit <- newButton main [text "Quit", width 15]
    pack quit [Side AtBottom, PadX 10, PadY 5]

    clickedquit <- clicked quit
    spawnEvent (forever (clickedquit >>> destroy main
                      +> tlev >>>= (\ev-> putStrLn ("TreeListEvent: "++
                                                     prtEv ev))))
    finishHTk

prtEv ::TreeListEvent FileObject-> String
prtEv (Selected Nothing)   = "nothing selected"
prtEv (Selected (Just tlo)) =
      let FileObject p nm _ = getTreeListObjectValue tlo in nm ++ " selected"
prtEv (Focused (Nothing, _))    = "nothing focussed"
prtEv (Focused (Just tlo, _)) =
      let FileObject p nm _ = getTreeListObjectValue tlo in nm ++ " focussed"

delEv :: TreeList FileObject-> TreeListEvent FileObject-> IO ()
delEv tl (Selected (Just tlo)) =
  removeTreeListObject tl (getTreeListObjectValue tlo)
delEv tl _ = done

folderImg = newImage [imgData GIF "R0lGODdhDAAMAPEAAP///4CAgP//AAAAACwAAAAADAAMAAACJ4SPGZsXYkKTQMDFAJ1DVwNVQUdZ1UV+qjB659uWkBlj9tIBw873BQA7"]
