{- ------------------------------------------------------------------------
 -
 - HTk Examples: Tree list example
 -
 - Author: ludi
 - $Revision$ from $Date$  
 -
 - ------------------------------------------------------------------------ -}

module Main (main) where

import HTk
import Concurrency
import Image
import TreeList

cfun :: ChildrenFun
cfun treelist obj =
  do
    objname <- getName obj
    case objname of
      "/"     -> do
                   obj1 <- newTreeListObject treelist [name "home"]
                   obj2 <- newTreeListObject treelist [name "usr"]
                   obj3 <- newTreeListObject treelist [name "tmp"]
                   return [obj1, obj2, obj3]
      "home"  -> do
                   obj1 <- newTreeListObject treelist [name "ludi"]
                   return [obj1]
      "usr"   -> do
                   obj1 <- newTreeListObject treelist [name "bin"]
                   obj2 <- newTreeListObject treelist [name "lib"]
                   obj3 <- newTreeListObject treelist [name "share"]
                   return [obj1, obj2, obj3]
      "tmp"   -> return []
      "ludi"  -> return []
      "bin"   -> return []
      "lib"   -> return []
      "share" -> return []

ifun :: ImageFun
ifun obj =
  do
    img <- newImage [filename "./images/folder.gif"]
    return img

main =
  do
    win <- htk []
    main <- newVFBox []
    box <- newHFBox [parent main]
    win <- window main [text "Tree list example"]
    treelist <- newTreeList cfun ifun [parent box, background "white",
                                       size (cm 15, cm 10)]
    obj <- newTreeListObject treelist [name "/"]
    setRoot treelist obj
    sync (destroyed win)
