{- Datatype for describing the contents of an extent, and functions for
   parsing it. -}
module EmacsContent(
   EmacsDataItem(..),
   EmacsContent(..),
   parseEmacsContent,
   parseEmacsContentGeneral,
   collapseEmacsContent,
   toEmacsLinks,
   ) where

import Maybe

import ExtendedPrelude(HasMapMonadic(..))

import EmacsSExp

data EmacsDataItem linkType =
      EmacsLink linkType -- key to some other emacs object
   |  EditableText String -- key to text to be edited
   deriving (Show)
   
newtype EmacsContent linkType = EmacsContent [EmacsDataItem linkType]   
   deriving (Show)

instance Functor EmacsContent where
   fmap fn (EmacsContent dataItems) =
      EmacsContent (map 
         (\ dataItem -> case dataItem of
            EditableText text -> EditableText text
            EmacsLink link -> EmacsLink (fn link)
            )
         dataItems
         )

instance HasMapMonadic EmacsContent where
   mapMonadic act (EmacsContent dataItems0) =
      do
         dataItems1 <- mapMonadic
            (\ dataItem -> case dataItem of
               EditableText text -> return (EditableText text)
               EmacsLink link0 ->
                  do
                     link1 <- act link0
                     return (EmacsLink link1)
               )
            dataItems0
         return (EmacsContent dataItems1)

-- | Extract the links from an EmacsContent
toEmacsLinks :: EmacsContent l -> [l]
toEmacsLinks (EmacsContent dataItems) =
   mapMaybe
      (\ dataItem -> case dataItem of
         EditableText _ -> Nothing
         EmacsLink link -> Just link
         )
      dataItems


-- | Extract the contents as returned from sendmess.el\'s uni-container-contents
-- function
parseEmacsContent :: String -> EmacsContent String
parseEmacsContent str =
   fmap (\ (_,str) -> str) (parseEmacsContentGeneral str)

-- | Extract the contents as returned from sendmess.el\'s uni-container-contents
-- and uni-container-children.  The Bool is True for included containers;
-- False for included buttons.
parseEmacsContentGeneral :: String -> EmacsContent (Bool,String)
parseEmacsContentGeneral str =
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
                     "button" -> Just (EmacsLink (False,s))
                     "container" -> Just (EmacsLink (True,s))
                     _ -> Nothing
               List [Id "uneditable-extent"] -> Nothing
               _ -> parsingError 2
               )
            list
   in
      EmacsContent dataItems

-- | Collapse adjacent EditableText parts together.
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
      

