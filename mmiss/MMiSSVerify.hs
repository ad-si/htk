{- This module contains the code for verifying that XML input matches a 
   given DTD.

   Currently the method employed is extremely clumsy and probably very slow.  
   The DTD must be compiled via DtdToHaskell to a Haskell file DTD_MMiSS.hs 
   which contains corresponding Haskell types and instances for parsing them.
   We use these instances to map from the given Content and back.
   We check that the resulting content is all defined.

   Hopefully a better method will exist one day. -}
module MMiSSVerify(
   DTDItem, -- corresponds to type in the DTD.
   toDTDItem, 
      -- :: XmlContent item => item -> DTDItem
      -- Give this an item with one of the types in DTD_MMiSS
      -- The value of the item is not looked at.
   verifyDTDItem,
      -- :: DTDItem -> Element -> IO (Maybe String)
      -- If Nothing, the element matches.  Otherwise returns an error message.
   ) where

import Exception
import Xml2Haskell
import XmlTypes

import DeepSeq

import DTD_MMiSS

data DTDItem = forall item . XmlContent item => DTDItem item

toDTDItem :: XmlContent item => item -> DTDItem
toDTDItem item = DTDItem item

-- If Nothing, the element matches.  Otherwise returns an error message.
verifyDTDItem :: DTDItem -> Element -> IO (Maybe String)
verifyDTDItem (DTDItem item) element =
   do
      let
         processedItem = unpackDTDItem item element
         unprocessedItem = toElem processedItem
      Exception.catch (evaluate (unprocessedItem `deepSeq` Nothing))
         (\ exception ->
            case exception of
               ErrorCall parseError -> 
                  return (Just ("Parsing error: "++parseError))
               _ -> return (Just ("Mysterious error: "++show exception))
            )

unpackDTDItem :: XmlContent item => item -> Element -> item
unpackDTDItem _ (el @ (Elem name _ _)) =
   case fromElem [CElem el] of
      (Just item,[]) -> item
      (Nothing,[]) -> error ("unexpected tag "++name++"??")
      (_,_:_) -> error ("unexpected junk at end of document")

-- ------------------------------------------------------------------------
-- We need instances of DeepSeq for Element/Content.
-- ------------------------------------------------------------------------

instance DeepSeq Element where
   deepSeq (Elem name attributes content) = deepSeq (name,attributes,content)

instance DeepSeq AttValue where
   deepSeq (AttValue l) = deepSeq l

instance DeepSeq XmlTypes.Reference where
   deepSeq (RefEntity e) = deepSeq e
   deepSeq (RefChar c) = deepSeq c

instance DeepSeq Content where
   deepSeq (CElem el) = deepSeq el
   deepSeq (CString b c) = deepSeq (b,c)
   deepSeq (CRef r) = deepSeq r
   deepSeq (CMisc m) = deepSeq m

instance DeepSeq Misc where
   deepSeq (Comment c) = deepSeq c
   deepSeq (PI p) = deepSeq p




