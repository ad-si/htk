module Server.Hosts where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

newtype Hosts = Hosts [Host]            deriving (Eq,Show)
data Host = Host
    { hostHostName :: String
    , hostPort :: (Defaultable String)
    , hostDescription :: (Maybe String)
    , hostUser :: (Maybe String)
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Hosts where
    fromElem (CElem (Elem "hosts" [] c0):rest) =
        (\(a,ca)->
           (Just (Hosts a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Hosts a) =
        [CElem (Elem "hosts" [] (concatMap toElem a))]
instance XmlContent Host where
    fromElem (CElem (Elem "host" as []):rest) =
        (Just (fromAttrs as), rest)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem as =
        [CElem (Elem "host" (toAttrs as) [])]
instance XmlAttributes Host where
    fromAttrs as =
        Host
          { hostHostName = definiteA fromAttrToStr "host" "hostName" as
          , hostPort = defaultA fromAttrToStr "11393" "port" as
          , hostDescription = possibleA fromAttrToStr "description" as
          , hostUser = possibleA fromAttrToStr "user" as
          }
    toAttrs v = catMaybes
        [ toAttrFrStr "hostName" (hostHostName v)
        , defaultToAttr toAttrFrStr "port" (hostPort v)
        , maybeToAttr toAttrFrStr "description" (hostDescription v)
        , maybeToAttr toAttrFrStr "user" (hostUser v)
        ]


{-Done-}
