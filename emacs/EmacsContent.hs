{- Datatype for describing the contents of an extent, and functions for
   parsing it. -}
module EmacsContent(
   EmacsDataItem(..),
   EmacsContent(..),
   parseEmacsContent,
   ) where

import Maybe

import EmacsSExp

data EmacsDataItem linkType =
      EmacsLink linkType -- key to some other emacs object
   |  EditableText String -- key to text to be edited
   deriving (Show)
   
newtype EmacsContent linkType = EmacsContent [EmacsDataItem linkType]   
   deriving (Show)

---
-- Extract the contents as returned from sendmess.el's uni-container-contents
-- function
parseEmacsContent :: String -> EmacsContent String
parseEmacsContent str =
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
      EmacsContent dataItems      

parsingError :: Int -> a
parsingError i = error ("EmacsContent error "++show i)
      

