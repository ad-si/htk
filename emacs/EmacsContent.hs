{- Datatype for describing the contents of an extent, and functions for
   parsing it. -}
module EmacsContent(
   EmacsDataItem(..),
   EmacsContent(..),
   parseEmacsContent,
   collapseEmacsContent,
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
               List [Id "uneditable-extent"] -> Nothing
               _ -> parsingError 2
               )
            list
   in
      EmacsContent dataItems

---
-- Collapse adjacent EditableText parts together.
collapseEmacsContent :: EmacsContent linkType -> EmacsContent linkType
collapseEmacsContent (EmacsContent list :: EmacsContent linkType) =
   let
      collapse :: [String] -> [EmacsDataItem linkType] 
         -> [EmacsDataItem linkType] 
      collapse inhand (remaining@(first:rest)) =
         case (inhand,first) of
            (strs,EmacsLink l) 
               -> addText strs (EmacsLink l : collapse [] rest)
            (strs,EditableText t) -> collapse (t:strs) rest
      collapse strs [] = addText strs []

      addText :: [String] -> [EmacsDataItem linkType] 
         -> [EmacsDataItem linkType]
      addText strs dataItems =
         case concat (reverse strs) of
            [] -> dataItems
            str -> EditableText str : dataItems
   in
      EmacsContent (collapse [] list)
 
parsingError :: Int -> a
parsingError i = error ("EmacsContent error "++show i)
      

