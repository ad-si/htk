module Text.XML.HaXml.OneOf40 where

import Text.XML.HaXml.Xml2Haskell

data OneOf40 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an
    = OneOf40 a | TwoOf40 b | ThreeOf40 c | FourOf40 d | FiveOf40 e
    | SixOf40 f | SevenOf40 g | EightOf40 h | NineOf40 i | TenOf40 j
    | ElevenOf40 k | TwelveOf40 l | ThirteenOf40 m | FourteenOf40 n
    | FifteenOf40 o | SixteenOf40 p | SeventeenOf40 q | EighteenOf40 r
    | NineteenOf40 s | TwentyOf40 t | Choice21Of40 u | Choice22Of40 v
    | Choice23Of40 w | Choice24Of40 x | Choice25Of40 y | Choice26Of40 z
    | Choice27Of40 aa | Choice28Of40 ab | Choice29Of40 ac | Choice30Of40 ad
    | Choice31Of40 ae | Choice32Of40 af | Choice33Of40 ag | Choice34Of40 ah
    | Choice35Of40 ai | Choice36Of40 aj | Choice37Of40 ak | Choice38Of40 al
    | Choice39Of40 am | Choice40Of40 an
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an)
    => XmlContent (OneOf40 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an)
  where
    fromElem cs =
        (choice OneOf40 $ choice TwoOf40 $ choice ThreeOf40 $ choice FourOf40
        $ choice FiveOf40 $ choice SixOf40 $ choice SevenOf40
        $ choice EightOf40 $ choice NineOf40 $ choice TenOf40
        $ choice ElevenOf40 $ choice TwelveOf40 $ choice ThirteenOf40
        $ choice FourteenOf40 $ choice FifteenOf40 $ choice SixteenOf40
        $ choice SeventeenOf40 $ choice EighteenOf40 $ choice NineteenOf40
        $ choice TwentyOf40 $ choice Choice21Of40 $ choice Choice22Of40
        $ choice Choice23Of40 $ choice Choice24Of40 $ choice Choice25Of40
        $ choice Choice26Of40 $ choice Choice27Of40 $ choice Choice28Of40
        $ choice Choice29Of40 $ choice Choice30Of40 $ choice Choice31Of40
        $ choice Choice32Of40 $ choice Choice33Of40 $ choice Choice34Of40
        $ choice Choice35Of40 $ choice Choice36Of40 $ choice Choice37Of40
        $ choice Choice38Of40 $ choice Choice39Of40 $ choice Choice40Of40
        $ (\c->(Nothing,c))) cs
    toElem (OneOf40 x) = toElem x
    toElem (TwoOf40 x) = toElem x
    toElem (ThreeOf40 x) = toElem x
    toElem (FourOf40 x) = toElem x
    toElem (FiveOf40 x) = toElem x
    toElem (SixOf40 x) = toElem x
    toElem (SevenOf40 x) = toElem x
    toElem (EightOf40 x) = toElem x
    toElem (NineOf40 x) = toElem x
    toElem (TenOf40 x) = toElem x
    toElem (ElevenOf40 x) = toElem x
    toElem (TwelveOf40 x) = toElem x
    toElem (ThirteenOf40 x) = toElem x
    toElem (FourteenOf40 x) = toElem x
    toElem (FifteenOf40 x) = toElem x
    toElem (SixteenOf40 x) = toElem x
    toElem (SeventeenOf40 x) = toElem x
    toElem (EighteenOf40 x) = toElem x
    toElem (NineteenOf40 x) = toElem x
    toElem (TwentyOf40 x) = toElem x
    toElem (Choice21Of40 x) = toElem x
    toElem (Choice22Of40 x) = toElem x
    toElem (Choice23Of40 x) = toElem x
    toElem (Choice24Of40 x) = toElem x
    toElem (Choice25Of40 x) = toElem x
    toElem (Choice26Of40 x) = toElem x
    toElem (Choice27Of40 x) = toElem x
    toElem (Choice28Of40 x) = toElem x
    toElem (Choice29Of40 x) = toElem x
    toElem (Choice30Of40 x) = toElem x
    toElem (Choice31Of40 x) = toElem x
    toElem (Choice32Of40 x) = toElem x
    toElem (Choice33Of40 x) = toElem x
    toElem (Choice34Of40 x) = toElem x
    toElem (Choice35Of40 x) = toElem x
    toElem (Choice36Of40 x) = toElem x
    toElem (Choice37Of40 x) = toElem x
    toElem (Choice38Of40 x) = toElem x
    toElem (Choice39Of40 x) = toElem x
    toElem (Choice40Of40 x) = toElem x

----
