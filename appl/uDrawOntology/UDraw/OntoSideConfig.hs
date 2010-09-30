{------------------------------------
MODULE        : UDraw.OntoSideConfig
AUTHOR        : Simon Drees,
                University of Bremen
DATE          : 2006
VERSION       : 1.0
DESCRIPTION   : Provides functions to access config-file of uDraw (Ontology).
-------------------------------------}

module UDraw.OntoSideConfig where

import Text.XML.HaXml.Xml2Haskell
-- import Text.XML.HaXml.OneOfN
import Data.Char (isSpace)


{-Type decls-}

data Config = Config Nodes Relations Packages
            deriving (Eq,Show)
newtype Nodes = Nodes [Node]            deriving (Eq,Show)
newtype Relations = Relations [Relation]                deriving (Eq,Show)
newtype Packages = Packages [Package]           deriving (Eq,Show)
data Node = Node
    { nodeType :: String
    , nodeFontStyle :: String
    , nodeFontFamily :: String
    , nodeBorder :: String
    , nodeShape :: String
    } deriving (Eq,Show)
data Relation = Relation
    { relationLabel :: String
    , relationShowLabel :: String
    , relationFontStyle :: String
    , relationFontFamily :: String
    , relationColor :: String
    , relationEdgePattern :: String
    , relationHead :: String
    } deriving (Eq,Show)
data Package = Package
    { packageLabel :: String
    , packageColor :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Config where
    fromElem (CElem (Elem "config" [] c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (\(c,cc)->
                 (Just (Config a b c), rest))
              (definite fromElem "<packages>" "config" cb))
           (definite fromElem "<relations>" "config" ca))
        (definite fromElem "<nodes>" "config" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Config a b c) =
        [CElem (Elem "config" [] (toElem a ++ toElem b ++ toElem c))]
instance XmlContent Nodes where
    fromElem (CElem (Elem "nodes" [] c0):rest) =
        (\(a,ca)->
           (Just (Nodes a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Nodes a) =
        [CElem (Elem "nodes" [] (concatMap toElem a))]
instance XmlContent Relations where
    fromElem (CElem (Elem "relations" [] c0):rest) =
        (\(a,ca)->
           (Just (Relations a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Relations a) =
        [CElem (Elem "relations" [] (concatMap toElem a))]
instance XmlContent Packages where
    fromElem (CElem (Elem "packages" [] c0):rest) =
        (\(a,ca)->
           (Just (Packages a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Packages a) =
        [CElem (Elem "packages" [] (concatMap toElem a))]
instance XmlContent Node where
    fromElem (CElem (Elem "node" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "node" (toAttrs as) [])]
instance XmlAttributes Node where
    fromAttrs as =
        Node
          { nodeType = definiteA fromAttrToStr "node" "type" as
          , nodeFontStyle = definiteA fromAttrToStr "node" "fontStyle" as
          , nodeFontFamily = definiteA fromAttrToStr "node" "fontFamily" as
          , nodeBorder = definiteA fromAttrToStr "node" "border" as
          , nodeShape = definiteA fromAttrToStr "node" "shape" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "type" (nodeType v)
        , toAttrFrStr "fontStyle" (nodeFontStyle v)
        , toAttrFrStr "fontFamily" (nodeFontFamily v)
        , toAttrFrStr "border" (nodeBorder v)
        , toAttrFrStr "shape" (nodeShape v)
        ]
instance XmlContent Relation where
    fromElem (CElem (Elem "relation" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "relation" (toAttrs as) [])]
instance XmlAttributes Relation where
    fromAttrs as =
        Relation
          { relationLabel = definiteA fromAttrToStr "relation" "label" as
          , relationShowLabel = definiteA fromAttrToStr "relation" "showLabel" as
          , relationFontStyle = definiteA fromAttrToStr "relation" "fontStyle" as
          , relationFontFamily = definiteA fromAttrToStr "relation" "fontFamily" as
          , relationColor = definiteA fromAttrToStr "relation" "color" as
          , relationEdgePattern = definiteA fromAttrToStr "relation" "edgepattern" as
          , relationHead = definiteA fromAttrToStr "relation" "head" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "label" (relationLabel v)
        , toAttrFrStr "showLabel" (relationShowLabel v)
        , toAttrFrStr "fontStyle" (relationFontStyle v)
        , toAttrFrStr "fontFamily" (relationFontFamily v)
        , toAttrFrStr "color" (relationColor v)
        , toAttrFrStr "edgepattern" (relationEdgePattern v)
        , toAttrFrStr "head" (relationHead v)
        ]
instance XmlContent Package where
    fromElem (CElem (Elem "package" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "package" (toAttrs as) [])]
instance XmlAttributes Package where
    fromAttrs as =
        Package
          { packageLabel = definiteA fromAttrToStr "package" "label" as
          , packageColor = definiteA fromAttrToStr "package" "color" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "label" (packageLabel v)
        , toAttrFrStr "color" (packageColor v)
        ]


{-Done-}
