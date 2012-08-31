module Server.Hosts where

import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Types

{-Type decls-}

newtype Hosts = Hosts [Host]            deriving (Eq,Show)
data Host = Host
    { hostHostName :: String
    , hostPort :: (Defaultable String)
    , hostDescription :: (Maybe String)
    , hostUser :: (Maybe String)
    } deriving (Eq,Show)


{-Instance decls-}

instance HTypeable Hosts where
    toHType x = Defined "hosts" [] []
instance XmlContent Hosts where
    toContents (Hosts a) =
        [CElem (Elem (N "hosts") [] (concatMap toContents a)) ()]
    parseContents = do
        { e@(Elem _ [] _) <- element ["hosts"]
        ; interior e $ return (Hosts) `apply` many parseContents
        } `adjustErr` ("in <hosts>, "++)

instance HTypeable Host where
    toHType x = Defined "host" [] []
instance XmlContent Host where
    toContents as =
        [CElem (Elem (N "host") (toAttrs as) []) ()]
    parseContents = do
        { (Elem _ as []) <- element ["host"]
        ; return (fromAttrs as)
        } `adjustErr` ("in <host>, "++)
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
