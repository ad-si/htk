module Text.XML.HaXml.OneOf22 where

import Text.XML.HaXml.Xml2Haskell

data OneOf22 a b c d e f g h i j k l m n o p q r s t u v
    = OneOf22 a | TwoOf22 b | ThreeOf22 c | FourOf22 d | FiveOf22 e
    | SixOf22 f | SevenOf22 g | EightOf22 h | NineOf22 i | TenOf22 j
    | ElevenOf22 k | TwelveOf22 l | ThirteenOf22 m | FourteenOf22 n
    | FifteenOf22 o | SixteenOf22 p | SeventeenOf22 q | EighteenOf22 r
    | NineteenOf22 s | TwentyOf22 t | Choice21Of22 u | Choice22Of22 v
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v)
    => XmlContent (OneOf22 a b c d e f g h i j k l m n o p q r s t u v)
  where
    fromElem cs =
        (choice OneOf22 $ choice TwoOf22 $ choice ThreeOf22 $ choice FourOf22
        $ choice FiveOf22 $ choice SixOf22 $ choice SevenOf22
        $ choice EightOf22 $ choice NineOf22 $ choice TenOf22
        $ choice ElevenOf22 $ choice TwelveOf22 $ choice ThirteenOf22
        $ choice FourteenOf22 $ choice FifteenOf22 $ choice SixteenOf22
        $ choice SeventeenOf22 $ choice EighteenOf22 $ choice NineteenOf22
        $ choice TwentyOf22 $ choice Choice21Of22 $ choice Choice22Of22
        $ (\c->(Nothing,c))) cs
    toElem (OneOf22 x) = toElem x
    toElem (TwoOf22 x) = toElem x
    toElem (ThreeOf22 x) = toElem x
    toElem (FourOf22 x) = toElem x
    toElem (FiveOf22 x) = toElem x
    toElem (SixOf22 x) = toElem x
    toElem (SevenOf22 x) = toElem x
    toElem (EightOf22 x) = toElem x
    toElem (NineOf22 x) = toElem x
    toElem (TenOf22 x) = toElem x
    toElem (ElevenOf22 x) = toElem x
    toElem (TwelveOf22 x) = toElem x
    toElem (ThirteenOf22 x) = toElem x
    toElem (FourteenOf22 x) = toElem x
    toElem (FifteenOf22 x) = toElem x
    toElem (SixteenOf22 x) = toElem x
    toElem (SeventeenOf22 x) = toElem x
    toElem (EighteenOf22 x) = toElem x
    toElem (NineteenOf22 x) = toElem x
    toElem (TwentyOf22 x) = toElem x
    toElem (Choice21Of22 x) = toElem x
    toElem (Choice22Of22 x) = toElem x

----
