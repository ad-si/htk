{- Datatype for describing the contents of an extent, and functions for
   parsing it. -}
module ExtentContents(
   EmacsDataItem(..),
   EmacsContents(..),
   parseEmacsContents,
   ) where

import Maybe

import EmacsSExp

data EmacsDataItem linkType =
      EmacsLink linkType -- key to some other emacs object
   |  EditableText String -- key to text to be edited

newtype EmacsContents linkType = EmacsContents [EmacsDataItem linkType]

---
-- Extract the contents as returned from sendmess.el's uni-container-contents
-- function
parseEmacsContents :: String -> EmacsContents String
parseEmacsContents str =
   let
      sexp = doParse str
      list = case sexp of
         List l -> l
         _ -> parsingError 1
      dataItems =
         mapMaybe
            (\ sexp -> case sexp of
               String s -> Just (EditableText s)
               DotList ([Id objectType]) (String s) ->
                  case objectType of
                     "button" -> Just (EmacsLink s)
                     "container" -> Just (EmacsLink s)
                     _ -> Nothing
               _ -> parsingError 2
               )
            list
   in
      EmacsContents dataItems      

parsingError :: Int -> a
parsingError i = error ("ExtentContents error "++show i)
      

