-- | This module accesses the MMiSS DTD, which is always done on start-up. 
-- 
-- The format include "Processing Instructions", with target "MMiSSDisplay".
-- The attached strings should in turn have format
--    element-name : graphic-instructions.
-- (ignoring spaces).
-- 
-- The graphic instructions are interpreted, for now, by 
-- DisplayParms.readDisplay.
module MMiSSDTD(
   allElements,
   allLabelledElements,
   validateElement,
   validateElement0, -- :: Element -> [String]
   getDisplayInstruction,
   xmlParseCheck, -- :: String -> String -> IO (WithError Element)
   xmlParseWE, -- :: String -> String -> WithError Element
      -- variant of xmlParseCheck which cheats using unsafePerformIO
      -- (Should work anyway.)
   toExportableXml, -- :: Element -> String
   toUglyExportableXml, -- :: Element -> String

   -- General interface for reading in and validating against a DTD.
   MMiSSDTD,
   readDTD, -- :: FilePath -> IO MMiSSDTD
   simpleDTD, -- :: MMiSSDTD -> Element -> [String]
      -- return list of errors for Element (hopefully empty)
   ) where

import IO
import Maybe

import Data.FiniteMap
import System.IO.Unsafe
import Control.Exception

import WBFiles
import IOExtras
import ExtendedPrelude
import DeepSeq
import Computation

import DisplayParms

import Text.PrettyPrint
import Text.XML.HaXml.Pretty
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Validate
import Text.XML.HaXml.Escape


import MMiSSDTDAssumptions

-- -------------------------------------------------------------
-- The internal representation of a DTD
-- -------------------------------------------------------------

data MMiSSDTD = MMiSSDTD {
   simpleDTD :: Element -> [String],
   displayInstructions :: FiniteMap String String,
   elements :: [String],
   labelledElements :: [String]
   }

-- -------------------------------------------------------------
-- Reading in the DTD.
-- -------------------------------------------------------------


theDTD :: MMiSSDTD
theDTD = unsafePerformIO readTheDTD
{-# NOINLINE theDTD #-}

readTheDTD :: IO MMiSSDTD
readTheDTD =
   do
      filePathOpt <- getMMiSSDTD
      case filePathOpt of
         Nothing -> error "MMiSSDTD must be specified"
         Just filePath -> readDTD filePath

readDTD :: FilePath -> IO MMiSSDTD
readDTD filePath =
   do
      handleEither <- Control.Exception.try (openFile filePath ReadMode)
      let
         handle = case handleEither of
            Left excep ->
               error ("Error opening MMiSS DTD from "++filePath++":"++
                  show excep)
            Right handle -> handle
      dtdString <- hGetContents handle
      let 
         (dtd,markups) = case dtdParse ("MMiSSDTD"++filePath) dtdString of
            Just (dtd @ (DTD _ _ markups)) -> (dtd,markups) 
            _ -> error ("Error reading MMiSS DTD from "++filePath++
             ": couldn't parse it")

         simpleDTD = partialValidate dtd

         elements = [element | Element (ElementDecl element _) <- markups]
    
         processingInstructions = 
            [parseProcessingInstruction instruction |
               MarkupMisc (PI ("MMiSSDisplay",instruction)) <- markups]

         mmissDTD = MMiSSDTD {
            simpleDTD = simpleDTD,
            elements = elements,
            displayInstructions = listToFM processingInstructions,
            labelledElements = findLabelledElements dtd
            }                            
      return mmissDTD

parseProcessingInstruction :: String -> (String,String)
parseProcessingInstruction str =
   case splitToChar ':' str of
      Nothing -> error ("DTD processing instruction "++str++" has no colon")
      Just (str1,str2) -> (trimSpaces str1,trimSpaces str2)

-- -------------------------------------------------------------
-- Accessing the DTD
-- -------------------------------------------------------------

allElements :: [String]
allElements = elements theDTD

-- | allElements, filtering just those elements which have a \"label\"
-- attribute.
allLabelledElements :: [String]
allLabelledElements = labelledElements theDTD

getDisplayInstruction :: String -> NodeTypes a
getDisplayInstruction str =
   case lookupFM (displayInstructions theDTD) str of
      Nothing -> defaultNodeTypes
      Just instructions -> readDisplay instructions

validateElement :: String -> Element -> [String]
validateElement elementName (element @ (Elem name _ _)) =
   if name /= elementName 
      then
         ["Expected a "++elementName++" but found a "++name]
      else
         validateElement0 element

validateElement0 :: Element -> [String]
validateElement0 = simpleDTD theDTD

-- -------------------------------------------------------------
-- Parsing a String containing Xml, with error control.
-- We need to instance DeepSeq for Element
-- -------------------------------------------------------------

-- | The first argument is the file name; the second the string to
-- parse.
xmlParseCheck :: String -> String -> IO (WithError Element)
xmlParseCheck fName contents =
   do
      result <- catchErrorCalls (
         let
            (Document _ _ el) = xmlParse fName contents
         in
            el `deepSeq` return el
         )
      return (case result of
         Left parseError -> hasError ("Parse error: "++parseError)
         Right el -> hasValue (xmlUnEscape stdXmlEscaper el)
         )

-- | Another version of xmlParse which checks for errors.  
xmlParseWE :: String -> String -> WithError Element
xmlParseWE fName contents = unsafePerformIO (xmlParseCheck fName contents)

instance DeepSeq Element where
   deepSeq (Elem name attributes contents) =
      deepSeq (name,attributes,contents) 

instance DeepSeq AttValue where
   deepSeq (AttValue l) = deepSeq l

instance DeepSeq Reference where
   deepSeq (RefEntity e) = deepSeq e
   deepSeq (RefChar c) = deepSeq c
   
instance DeepSeq Content where
   deepSeq (CElem e) = deepSeq e
   deepSeq (CString b c) = deepSeq (b,c)
   deepSeq (CRef r) = deepSeq r
   deepSeq (CMisc m) = deepSeq m

instance DeepSeq Misc where
   deepSeq (Comment c) = deepSeq c
   deepSeq (PI p) = deepSeq p

-- -------------------------------------------------------------
-- UnParsing XML
-- -------------------------------------------------------------

toUglyExportableXml :: Element -> String
toUglyExportableXml elem =
   let
      style = Style {
         mode = OneLineMode,
         lineLength = 80,
         ribbonsPerLine = 1.5
         }
   in
       (renderStyle style) . element . (xmlEscape stdXmlEscaper) $ elem

toExportableXml :: Element -> String
toExportableXml elem = 
    render . element . (xmlEscape stdXmlEscaper) $ elem



      
    
