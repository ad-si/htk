{- This module kindly contributed by Malcolm Wallace.  Perhaps it will turn
   up in the HaXml distribution one day.

   I have made some fairly minor changes.  The original version can be found
   in XmlValidate.original.
   -}
module XmlValidate( 
  validate,
  ) where

import XmlTypes
import Maybe (fromMaybe,isNothing,fromJust)
import List (intersperse)
import Xml2Haskell (attr2str)
import FiniteMap

-- gather appropriate information out of the DTD
data SimpleDTD = SimpleDTD
    { elements   :: FiniteMap Name ContentSpec
    , attributes :: FiniteMap (Name,Name) AttType
    , required   :: FiniteMap Name [Name]	-- required attributes
    }


---
-- lookupAttribute simpleDTD obj attName
-- returns the attribute type for that attName on obj, if it exists
lookupAttribute :: SimpleDTD -> String -> String -> Maybe AttType
lookupAttribute simpleDTD obj attName 
   = lookupFM (attributes simpleDTD) (obj,attName)

simplifyDTD :: DocTypeDecl -> SimpleDTD
simplifyDTD (DTD _ _ decls) =
    SimpleDTD
        { elements   = listToFM [ (name,content)
                       | Element (ElementDecl name content) <- decls ]
        , attributes = listToFM [ ((elem,attr),typ)
                       | AttList (AttListDecl elem attdefs) <- decls
                       , AttDef attr typ _ <- attdefs ]
        , required   = listToFM [ (elem,attrs)
                       | AttList (AttListDecl elem attdefs) <- decls
                       , let attrs = [ attr
                                     | AttDef attr _ REQUIRED <- attdefs ] ]
        }

-- simple auxiliary to avoid lots of if-then-else with empty else clauses.
gives :: Bool -> a -> [a]
True `gives` x = [x]
False `gives` _ = []

-- 'validate' takes a DTD and a content element, and returns a list of
-- errors in the document with respect to its DTD.
validate :: DocTypeDecl -> Element -> [String]
validate dtd' elem = walk (CElem elem)
  where
    dtd = simplifyDTD dtd'

    walk (CElem (Elem name attrs contents)) =
        let spec = lookupFM (elements dtd) name 
            label = getLabel attrs
        in 
        (isNothing spec) `gives` ("Element <"++name++"> not known.")
        ++ concatMap (checkAttr name) attrs
        ++ concatMap (checkRequired name attrs)
                     (fromMaybe [] (lookupFM (required dtd) name))
        ++ checkContentSpec name label (fromMaybe ANY spec) contents
        ++ concatMap walk contents
    walk _ = []

    getLabel (("label", (AttValue [v])):as) = 
      case v of
        Left str -> str
        _ -> ""
    getLabel (a:as) = getLabel as                                             
    getLabel [] = ""

    checkAttr elem (attr, val) =
        let typ = lookupFM (attributes dtd) (elem,attr)
            attval = attr2str val in
        if isNothing typ then ["Attribute \""++attr
                               ++"\" not known for element <"++elem++">."]
        else
          case fromJust typ of
            EnumeratedType e ->
              case e of
                Enumeration es -> (not (attval `Prelude.elem` es)) `gives`
                                      ("Value \""++attval++"\" of attribute \""
                                       ++attr++"\" in element <"++elem
                                       ++"> is not in the required enumeration" ++
                                       " range: "++unwords es)
                _ -> []
            _ -> []

    checkRequired elem attrs req =
        (not (req `Prelude.elem` map fst attrs)) `gives`
            ("Element <"++elem++"> requires the attribute \""++req
             ++"\" but it is missing.")

    checkContentSpec elem label ANY _ = []
    checkContentSpec elem label EMPTY [] = []
    checkContentSpec elem label EMPTY (_:_) =
        ["Element <"++elem++"> with label '"++label++"' is not empty but should be."]
    checkContentSpec elem label (Mixed PCDATA) cs = concatMap (checkMixed elem label []) cs
    checkContentSpec elem label (Mixed (PCDATAplus names)) cs =
        concatMap (checkMixed elem label names) cs
    checkContentSpec elem label (ContentSpec cp) cs = excludeText elem label cs ++
        (let (errs,rest) = checkCP elem label cp (flatten cs) in
         case rest of [] -> errs
                      _  -> errs++["Element <"++elem++"> with label '"++label++"' contains more elements" ++
                                  " beyond its content spec."])

    checkMixed elem label permitted (CElem (Elem name _ _))
        | not (name `Prelude.elem` permitted) =
            ["Element <"++elem++"> with label '"++label++"' contains an element <"++name
             ++"> but should not."]
    checkMixed elem label permitted _ = []

    flatten (CElem (Elem name _ _): cs) = name: flatten cs
    flatten (_: cs)                     = flatten cs
    flatten []                          = []

    excludeText elem label (CElem _: cs) = excludeText elem cs
    excludeText elem label (CMisc _: cs) = excludeText elem cs
    excludeText elem label (_:  cs) =
        ["Element <"++elem++"> with label '"++label++"' contains text/references but should not."]
    excludeText elem [] = []

    -- This is a little parser really.  Returns errors, plus the remainder
    -- of the input string.
    checkCP :: Name -> String -> CP -> [Name] -> ([String],[Name])
    checkCP elem label cp@(TagName n None) [] = (cpError elem label cp, [])
    checkCP elem label cp@(TagName n None) (n':ns)
        | n==n'     = ([], ns)
        | otherwise = (cpError elem cp, n':ns)
    checkCP elem label cp@(TagName n Query) [] = ([],[])
    checkCP elem label cp@(TagName n Query) (n':ns)
        | n==n'     = ([], ns)
        | otherwise = ([], n':ns)
    checkCP elem label cp@(TagName n Star) [] = ([],[])
    checkCP elem label cp@(TagName n Star) (n':ns)
        | n==n'     = checkCP elem label (TagName n Star) ns
        | otherwise = ([], n':ns)
    checkCP elem label cp@(TagName n Plus) [] = (cpError elem label cp, [])
    checkCP elem label cp@(TagName n Plus) (n':ns)
        | n==n'     = checkCP elem label (TagName n Star) ns
        | otherwise = (cpError elem cp, n':ns)
    checkCP elem label cp@(Choice cps None) [] = (cpError elem label cp, [])
    checkCP elem label cp@(Choice cps None) ns =
        let next = [ rem | ([],rem) <- map (\cp-> checkCP elem label cp ns) cps ] in
        if null next then (cpError elem label cp, ns)
        else ([], head next)	-- choose the first alternative with no errors
    checkCP elem label cp@(Choice cps Query) [] = ([],[])
    checkCP elem label cp@(Choice cps Query) ns =
        let next = [ rem | ([],rem) <- map (\cp-> checkCP elem label cp ns) cps ] in
        if null next then ([],ns)
        else ([], head next)
    checkCP elem label cp@(Choice cps Star) [] = ([],[])
    checkCP elem label cp@(Choice cps Star) ns =
        let next = [ rem | ([],rem) <- map (\cp-> checkCP elem label cp ns) cps ] in
        if null next then ([],ns)
        else checkCP elem label (Choice cps Star) (head next)
    checkCP elem label cp@(Choice cps Plus) [] = (cpError elem label cp, [])
    checkCP elem label cp@(Choice cps Plus) ns =
        let next = [ rem | ([],rem) <- map (\cp-> checkCP elem label cp ns) cps ] in
        if null next then (cpError elem label cp, ns)
        else checkCP elem label (Choice cps Star) (head next)
    checkCP elem label cp@(Seq cps None) [] = (cpError elem label cp, [])
    checkCP elem label cp@(Seq cps None) ns =
        let (errs,next) = sequence elem label ns cps in
        if null errs then ([],next)
        else (cpError elem label cp++errs, ns)
    checkCP elem label cp@(Seq cps Query) [] = ([],[])
    checkCP elem label cp@(Seq cps Query) ns =
        let (errs,next) = sequence elem label ns cps in
        if null errs then ([],next)
        else ([], ns)
    checkCP elem label cp@(Seq cps Star) [] = ([],[])
    checkCP elem label cp@(Seq cps Star) ns =
        let (errs,next) = sequence elem label ns cps in
        if null errs then checkCP elem label (Seq cps Star) next
        else ([], ns)
    checkCP elem label cp@(Seq cps Plus) [] = (cpError elem label  cp, [])
    checkCP elem label cp@(Seq cps Plus) ns =
        let (errs,next) = sequence elem label ns cps in
        if null errs then checkCP elem label (Seq cps Star) next
        else (cpError elem label cp++errs, ns)

    sequence elem label ns cps =
        foldl (\(es,ns) cp-> let (es',ns') = checkCP elem label cp ns
                             in (es++es', ns'))
              ([],ns) cps


cpError :: Name -> String -> CP -> [String]
cpError elem label cp =
    ["Element <"++elem++"> with label '"++label++"' should contain "++display cp++" but does not."]


display :: CP -> String
display (TagName name mod) = name ++ modifier mod
display (Choice cps mod)   = "(" ++ concat (intersperse "|" (map display cps))
                             ++ ")" ++ modifier mod
display (Seq cps mod)      = "(" ++ concat (intersperse "," (map display cps))
                             ++ ")" ++ modifier mod

modifier :: Modifier -> String
modifier None  = ""
modifier Query = "?"
modifier Star  = "*"
modifier Plus  = "+"
