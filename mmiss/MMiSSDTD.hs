{- This module accesses the MMiSS DTD, which is always done on start-up. 

   The format include "Processing Instructions", with target "MMiSSDisplay".
   The attached strings should in turn have format
      element-name : graphic-instructions.
   (ignoring spaces).

   The graphic instructions are interpreted, for now, by 
   DisplayParms.readDisplay.
   -}
module MMiSSDTD(
   allElements,
   validateElement,
   getDisplayInstruction,
   ) where

import IO
import Maybe

import FiniteMap
import qualified IOExts (unsafePerformIO)

import XmlParse
import XmlTypes

import WBFiles
import ExtendedPrelude

import DisplayParms

import XmlValidate

-- -------------------------------------------------------------
-- The internal representation of a DTD
-- -------------------------------------------------------------

data MMiSSDTD = MMiSSDTD {
   simpleDTD :: SimpleDTD,
   displayInstructions :: FiniteMap String String,
   elements :: [String]
   }

-- -------------------------------------------------------------
-- Reading in the DTD.
-- -------------------------------------------------------------


theDTD :: MMiSSDTD
theDTD = IOExts.unsafePerformIO readTheDTD
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
      handle <- openFile filePath ReadMode
      dtdString <- hGetContents handle
      let 
         Just (dtd @ (DTD _ _ markups)) = dtdParse "MMiSSDTD" dtdString
         simpleDTD = simplifyDTD dtd

         elements = [element | Element (ElementDecl element _) <- markups]
    
         processingInstructions = 
            [parseProcessingInstruction instruction |
               MarkupMisc (PI ("MMiSSDisplay",instruction)) <- markups]

         mmissDTD = MMiSSDTD {
            simpleDTD = simpleDTD,
            elements = elements,
            displayInstructions = listToFM processingInstructions
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

getDisplayInstruction :: String -> NodeTypes a
getDisplayInstruction str =
   case lookupFM (displayInstructions theDTD) str of
      Nothing -> defaultNodeTypes
      Just instructions -> readDisplay instructions

validateElement :: String -> Element -> [String]
validateElement elementName (element @ (Elem name _ _)) =
   if name /= elementName 
      then
         ["Expected an "++elementName++" but found a "++name]
      else
          validate (simpleDTD theDTD) (CElem element)

    