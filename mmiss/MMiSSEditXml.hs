-- | This module contains functions for editing Xml 
module MMiSSEditXml(
   -- The first argument of each function is the file name.
   toEditableXml, 
       -- :: String -> Element -> EmacsContent (TypedName,[Attribute]) 
   fromEditableXml, -- :: String -> EmacsContent (TypedName,[Attribute]) 
     -- -> IO (WithError Element)
   parseXmlString, -- exported for debugging purposes

   toExportableXml, -- :: Element -> String
      -- actually defined in MMiSSDTD.

   TypedName,

   ) where

import Maybe

import Computation
import ExtendedPrelude
import Debug(debug)
import CompileFlags

import EmacsContent

import Text.XML.HaXml.Types
import Text.XML.HaXml.Lex

import MMiSSDTD
import MMiSSDTDAssumptions

type TypedName = (String,Char)

fromEditableXml :: String -> EmacsContent (TypedName,[Attribute]) 
   -> IO (WithError Element)
fromEditableXml fName (EmacsContent dataItems) =
   do
      let
         xmlString = concat (
            map
               (\ dataItem -> case dataItem of
                  EditableText text -> text
                  EmacsLink ((link,miniType),attributes) ->
                        "<include"++toIncludeStr miniType
                           ++" included=\"" ++ link++"\""
                           ++" status=\"present\""
                           ++printAttributes attributes
                           ++"/>"
                  )
               dataItems
            )
      xmlParseCheck fName xmlString 

toEditableXml :: String -> Element -> EmacsContent (TypedName,[Attribute])
toEditableXml fName elem =
   let
      xmlString = toExportableXml elem
   in
      parseXmlString xmlString

-- -------------------------------------------------------------------
-- Functions for extracting the includes from XML.
-- We use the HaXml lex functions, which save us the bother of comment-
-- and string- parsing.
--
-- The basic idea is that we tokenise the whole String, search for 
-- suitable includes, and use values of HaXml's Posn type to extract the
-- remaining strings.  There are probably more efficient ways.
--
-- NB.  We can at least assume that the string is well-formed.
-- -------------------------------------------------------------------

parseXmlString :: String -> EmacsContent (TypedName,[Attribute])
parseXmlString s0 =
   let
      -- (0) if isDebug is set, check that there aren't any funny characters
      -- which upset HaXml's column-numbering algorithm.

      funny :: Char -> Bool
      funny '\r' = True
      funny '\t' = True
      funny _ = False

      s = 
         if isDebug -- hopefully if not this will get optimised out.
            then
               if any funny s0 then 
                  error ("MMiSSEditXml: funny chars in "++show s) else s0
                  
            else s0

      -- (1) tokenize
      tokens = xmlLex "MMiSSEditXml.extractAllIncludes" s

      -- (2) get all closed tags
      closedTags1 = extractClosedTags tokens

      -- (3) get all includes
      closedTags2 = extractIncludes closedTags1

      -- (4) get all present includes 
      closedTags3 = extractPresent closedTags2

      -- (5) turn this into contents
      mkContents :: Posn -> String -> [ClosedTag]
          -> [EmacsDataItem (TypedName,[Attribute])]
      mkContents stringStart0 rest0 [] = addText rest0 []
      mkContents stringStart0 rest0 (closedTag:closedTags) =
         let
            start1 = start closedTag
            end1 = end closedTag

            -- Get the string up to start1.
            (text,rest1) = extractFromString stringStart0 start1 rest0

            -- Deconstruct the closed tag
            miniType = fromIncludeStr (name closedTag)

            Just id = lookup "included" (attributes closedTag)

            otherAttributes =
               mapMaybe
                  (\ (key,value) -> case key of
                     "status" -> Nothing
                     "included" -> Nothing
                     _ -> Just (key,AttValue [Left value])
                     )
                  (attributes closedTag)

            dataItem2 = EmacsLink ((id,miniType),otherAttributes)

            -- Get the other dataitems
            (_,'/':'>':rest2) = extractFromString start1 end1 rest1
            stringStart1 = stepString end1 "/>"

            dataItems0 = mkContents stringStart1 rest2 closedTags

            -- Put them together
            dataItems1 = dataItem2 : dataItems0
            dataItems2 = addText text dataItems1
         in
            dataItems2

      startPos = (Pn (error "MMiSSEditXml.1") 1 1 (error "MMiSSEditXml.2")) 
      dataItems = mkContents startPos s closedTags3

      addText :: String -> [EmacsDataItem (TypedName,[Attribute])]
         -> [EmacsDataItem (TypedName,[Attribute])]
      addText "" dataItems = dataItems
      addText text dataItems = EditableText text : dataItems
   in
      EmacsContent dataItems

                            
-- | Our strategy is to extract all ClosedTags (tags all of whose attributes
-- are Strings, ending with \/>) and then filter them gradually).
data ClosedTag = ClosedTag {
   name :: String,
   attributes :: [(String,String)],
   start :: Posn,
   end :: Posn
   }

extractClosedTags :: [Token] -> [ClosedTag]
extractClosedTags [] = []
extractClosedTags (token1:tokens) =
   case snd token1 of
      TokAnyOpen ->
         let
            -- (1) scan to close
            (Just (tagData,tokenEnd,restTokens)) = splitToElemGeneral
               (\ tok -> case snd tok of
                  TokEndClose -> True
                  TokAnyClose -> True
                  _ -> False
                  )
               tokens

            -- (2) what we will put afterwards
            rest = extractClosedTags restTokens
         in
            case snd tokenEnd of
               TokAnyClose -> rest 
                  -- fall out 1, ends with > not />
               TokEndClose -> case map snd tagData of
                  ((TokName name):attData) ->
                     let
                        getAttributes :: [TokenT] -> Maybe [(String,String)]
                        getAttributes [] = Just []
                        getAttributes (
                           TokName key : TokEqual : TokQuote 
                              : TokFreeText value : TokQuote
                              : restTokens) = 
                           fmap
                              (\ restAtts -> (key,value) : restAtts)
                              (getAttributes restTokens)
                     in
                        case getAttributes attData of
                           Nothing -> rest
                           Just attributes ->
                              let
                                 closedTag = ClosedTag {
                                    name = name,
                                    attributes = attributes,
                                    start = fst token1,
                                    end = fst tokenEnd
                                    }
                              in
                                 closedTag : rest
      _ -> extractClosedTags tokens
            
-- | Extract all tags which have names beginning \"include\", and for those
-- remove the include.
extractIncludes :: [ClosedTag] -> [ClosedTag]
extractIncludes = mapMaybe
   (\ closedTag -> case name closedTag of
      'i':'n':'c':'l':'u':'d':'e':name1 -> Just (closedTag {name = name1})
      _ -> Nothing
      )

-- | Extract all tags which have status=present and an included attribute set.
extractPresent :: [ClosedTag] -> [ClosedTag]
extractPresent = filter
   (\ closedTag -> case
         (lookup "status" (attributes closedTag),
            lookup "included" (attributes closedTag)) of
      (Just "present",Just _) -> True
      _ -> False
      )

-- | extractFromString extractStart extractEnd string
-- assumes the string begins at position extractStart, extracts
-- the portion of it up until extractEnd-1, and
-- returns the string starting at extractEnd as well. 
extractFromString :: Posn -> Posn -> String -> (String,String)
extractFromString extractStart extractEnd string =
   case (comp extractStart extractEnd,string) of
      (EQ,_) -> ([],string)
      (LT,c:cs) ->
         let
            next = step extractStart c
            (extracted,rest) = extractFromString next extractEnd cs
         in
            (c:extracted,rest)
   where
      comp :: Posn -> Posn -> Ordering
      comp (Pn _ l1 c1 _) (Pn _ l2 c2 _) =
         case compare l1 l2 of
            LT -> LT
            GT -> GT
            EQ -> compare c1 c2

-- | Given that the start of s is at posn, return the position after s.
stepString :: Posn -> String -> Posn
stepString posn s = case s of
   [] -> posn
   (c:rest) -> stepString (step posn c) rest

-- | Given that the position at char is at posn, this returns
-- the position of the following character.  Each line is deemed to
-- end with its newline character.
step :: Posn -> Char -> Posn
step (Pn x l c y) ch =
   case ch of
      '\n' -> Pn x (l+1) 1 y
      _ -> Pn x l (c+1) y

