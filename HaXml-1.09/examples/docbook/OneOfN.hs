module Text.XML.HaXml.OneOfN where

import Text.XML.HaXml.Xml2Haskell

data OneOf2 a b
    = OneOf2 a | TwoOf2 b
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b)
    => XmlContent (OneOf2 a b)
  where
    fromElem cs =
        (choice OneOf2 $ choice TwoOf2
        $ (\c->(Nothing,c))) cs
    toElem (OneOf2 x) = toElem x
    toElem (TwoOf2 x) = toElem x

----
data OneOf3 a b c
    = OneOf3 a | TwoOf3 b | ThreeOf3 c
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c)
    => XmlContent (OneOf3 a b c)
  where
    fromElem cs =
        (choice OneOf3 $ choice TwoOf3 $ choice ThreeOf3
        $ (\c->(Nothing,c))) cs
    toElem (OneOf3 x) = toElem x
    toElem (TwoOf3 x) = toElem x
    toElem (ThreeOf3 x) = toElem x

----
data OneOf4 a b c d
    = OneOf4 a | TwoOf4 b | ThreeOf4 c | FourOf4 d
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d)
    => XmlContent (OneOf4 a b c d)
  where
    fromElem cs =
        (choice OneOf4 $ choice TwoOf4 $ choice ThreeOf4 $ choice FourOf4
        $ (\c->(Nothing,c))) cs
    toElem (OneOf4 x) = toElem x
    toElem (TwoOf4 x) = toElem x
    toElem (ThreeOf4 x) = toElem x
    toElem (FourOf4 x) = toElem x

----
data OneOf5 a b c d e
    = OneOf5 a | TwoOf5 b | ThreeOf5 c | FourOf5 d | FiveOf5 e
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e)
    => XmlContent (OneOf5 a b c d e)
  where
    fromElem cs =
        (choice OneOf5 $ choice TwoOf5 $ choice ThreeOf5 $ choice FourOf5
        $ choice FiveOf5
        $ (\c->(Nothing,c))) cs
    toElem (OneOf5 x) = toElem x
    toElem (TwoOf5 x) = toElem x
    toElem (ThreeOf5 x) = toElem x
    toElem (FourOf5 x) = toElem x
    toElem (FiveOf5 x) = toElem x

----
data OneOf6 a b c d e f
    = OneOf6 a | TwoOf6 b | ThreeOf6 c | FourOf6 d | FiveOf6 e | SixOf6 f
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f)
    => XmlContent (OneOf6 a b c d e f)
  where
    fromElem cs =
        (choice OneOf6 $ choice TwoOf6 $ choice ThreeOf6 $ choice FourOf6
        $ choice FiveOf6 $ choice SixOf6
        $ (\c->(Nothing,c))) cs
    toElem (OneOf6 x) = toElem x
    toElem (TwoOf6 x) = toElem x
    toElem (ThreeOf6 x) = toElem x
    toElem (FourOf6 x) = toElem x
    toElem (FiveOf6 x) = toElem x
    toElem (SixOf6 x) = toElem x

----
data OneOf7 a b c d e f g
    = OneOf7 a | TwoOf7 b | ThreeOf7 c | FourOf7 d | FiveOf7 e | SixOf7 f
    | SevenOf7 g
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g)
    => XmlContent (OneOf7 a b c d e f g)
  where
    fromElem cs =
        (choice OneOf7 $ choice TwoOf7 $ choice ThreeOf7 $ choice FourOf7
        $ choice FiveOf7 $ choice SixOf7 $ choice SevenOf7
        $ (\c->(Nothing,c))) cs
    toElem (OneOf7 x) = toElem x
    toElem (TwoOf7 x) = toElem x
    toElem (ThreeOf7 x) = toElem x
    toElem (FourOf7 x) = toElem x
    toElem (FiveOf7 x) = toElem x
    toElem (SixOf7 x) = toElem x
    toElem (SevenOf7 x) = toElem x

----
data OneOf8 a b c d e f g h
    = OneOf8 a | TwoOf8 b | ThreeOf8 c | FourOf8 d | FiveOf8 e | SixOf8 f
    | SevenOf8 g | EightOf8 h
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h)
    => XmlContent (OneOf8 a b c d e f g h)
  where
    fromElem cs =
        (choice OneOf8 $ choice TwoOf8 $ choice ThreeOf8 $ choice FourOf8
        $ choice FiveOf8 $ choice SixOf8 $ choice SevenOf8 $ choice EightOf8
        $ (\c->(Nothing,c))) cs
    toElem (OneOf8 x) = toElem x
    toElem (TwoOf8 x) = toElem x
    toElem (ThreeOf8 x) = toElem x
    toElem (FourOf8 x) = toElem x
    toElem (FiveOf8 x) = toElem x
    toElem (SixOf8 x) = toElem x
    toElem (SevenOf8 x) = toElem x
    toElem (EightOf8 x) = toElem x

----
data OneOf9 a b c d e f g h i
    = OneOf9 a | TwoOf9 b | ThreeOf9 c | FourOf9 d | FiveOf9 e | SixOf9 f
    | SevenOf9 g | EightOf9 h | NineOf9 i
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i)
    => XmlContent (OneOf9 a b c d e f g h i)
  where
    fromElem cs =
        (choice OneOf9 $ choice TwoOf9 $ choice ThreeOf9 $ choice FourOf9
        $ choice FiveOf9 $ choice SixOf9 $ choice SevenOf9 $ choice EightOf9
        $ choice NineOf9
        $ (\c->(Nothing,c))) cs
    toElem (OneOf9 x) = toElem x
    toElem (TwoOf9 x) = toElem x
    toElem (ThreeOf9 x) = toElem x
    toElem (FourOf9 x) = toElem x
    toElem (FiveOf9 x) = toElem x
    toElem (SixOf9 x) = toElem x
    toElem (SevenOf9 x) = toElem x
    toElem (EightOf9 x) = toElem x
    toElem (NineOf9 x) = toElem x

----
data OneOf10 a b c d e f g h i j
    = OneOf10 a | TwoOf10 b | ThreeOf10 c | FourOf10 d | FiveOf10 e
    | SixOf10 f | SevenOf10 g | EightOf10 h | NineOf10 i | TenOf10 j
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j)
    => XmlContent (OneOf10 a b c d e f g h i j)
  where
    fromElem cs =
        (choice OneOf10 $ choice TwoOf10 $ choice ThreeOf10 $ choice FourOf10
        $ choice FiveOf10 $ choice SixOf10 $ choice SevenOf10
        $ choice EightOf10 $ choice NineOf10 $ choice TenOf10
        $ (\c->(Nothing,c))) cs
    toElem (OneOf10 x) = toElem x
    toElem (TwoOf10 x) = toElem x
    toElem (ThreeOf10 x) = toElem x
    toElem (FourOf10 x) = toElem x
    toElem (FiveOf10 x) = toElem x
    toElem (SixOf10 x) = toElem x
    toElem (SevenOf10 x) = toElem x
    toElem (EightOf10 x) = toElem x
    toElem (NineOf10 x) = toElem x
    toElem (TenOf10 x) = toElem x

----
data OneOf11 a b c d e f g h i j k
    = OneOf11 a | TwoOf11 b | ThreeOf11 c | FourOf11 d | FiveOf11 e
    | SixOf11 f | SevenOf11 g | EightOf11 h | NineOf11 i | TenOf11 j
    | ElevenOf11 k
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k)
    => XmlContent (OneOf11 a b c d e f g h i j k)
  where
    fromElem cs =
        (choice OneOf11 $ choice TwoOf11 $ choice ThreeOf11 $ choice FourOf11
        $ choice FiveOf11 $ choice SixOf11 $ choice SevenOf11
        $ choice EightOf11 $ choice NineOf11 $ choice TenOf11
        $ choice ElevenOf11
        $ (\c->(Nothing,c))) cs
    toElem (OneOf11 x) = toElem x
    toElem (TwoOf11 x) = toElem x
    toElem (ThreeOf11 x) = toElem x
    toElem (FourOf11 x) = toElem x
    toElem (FiveOf11 x) = toElem x
    toElem (SixOf11 x) = toElem x
    toElem (SevenOf11 x) = toElem x
    toElem (EightOf11 x) = toElem x
    toElem (NineOf11 x) = toElem x
    toElem (TenOf11 x) = toElem x
    toElem (ElevenOf11 x) = toElem x

----
data OneOf12 a b c d e f g h i j k l
    = OneOf12 a | TwoOf12 b | ThreeOf12 c | FourOf12 d | FiveOf12 e
    | SixOf12 f | SevenOf12 g | EightOf12 h | NineOf12 i | TenOf12 j
    | ElevenOf12 k | TwelveOf12 l
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l)
    => XmlContent (OneOf12 a b c d e f g h i j k l)
  where
    fromElem cs =
        (choice OneOf12 $ choice TwoOf12 $ choice ThreeOf12 $ choice FourOf12
        $ choice FiveOf12 $ choice SixOf12 $ choice SevenOf12
        $ choice EightOf12 $ choice NineOf12 $ choice TenOf12
        $ choice ElevenOf12 $ choice TwelveOf12
        $ (\c->(Nothing,c))) cs
    toElem (OneOf12 x) = toElem x
    toElem (TwoOf12 x) = toElem x
    toElem (ThreeOf12 x) = toElem x
    toElem (FourOf12 x) = toElem x
    toElem (FiveOf12 x) = toElem x
    toElem (SixOf12 x) = toElem x
    toElem (SevenOf12 x) = toElem x
    toElem (EightOf12 x) = toElem x
    toElem (NineOf12 x) = toElem x
    toElem (TenOf12 x) = toElem x
    toElem (ElevenOf12 x) = toElem x
    toElem (TwelveOf12 x) = toElem x

----
data OneOf13 a b c d e f g h i j k l m
    = OneOf13 a | TwoOf13 b | ThreeOf13 c | FourOf13 d | FiveOf13 e
    | SixOf13 f | SevenOf13 g | EightOf13 h | NineOf13 i | TenOf13 j
    | ElevenOf13 k | TwelveOf13 l | ThirteenOf13 m
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m)
    => XmlContent (OneOf13 a b c d e f g h i j k l m)
  where
    fromElem cs =
        (choice OneOf13 $ choice TwoOf13 $ choice ThreeOf13 $ choice FourOf13
        $ choice FiveOf13 $ choice SixOf13 $ choice SevenOf13
        $ choice EightOf13 $ choice NineOf13 $ choice TenOf13
        $ choice ElevenOf13 $ choice TwelveOf13 $ choice ThirteenOf13
        $ (\c->(Nothing,c))) cs
    toElem (OneOf13 x) = toElem x
    toElem (TwoOf13 x) = toElem x
    toElem (ThreeOf13 x) = toElem x
    toElem (FourOf13 x) = toElem x
    toElem (FiveOf13 x) = toElem x
    toElem (SixOf13 x) = toElem x
    toElem (SevenOf13 x) = toElem x
    toElem (EightOf13 x) = toElem x
    toElem (NineOf13 x) = toElem x
    toElem (TenOf13 x) = toElem x
    toElem (ElevenOf13 x) = toElem x
    toElem (TwelveOf13 x) = toElem x
    toElem (ThirteenOf13 x) = toElem x

----
data OneOf14 a b c d e f g h i j k l m n
    = OneOf14 a | TwoOf14 b | ThreeOf14 c | FourOf14 d | FiveOf14 e
    | SixOf14 f | SevenOf14 g | EightOf14 h | NineOf14 i | TenOf14 j
    | ElevenOf14 k | TwelveOf14 l | ThirteenOf14 m | FourteenOf14 n
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n)
    => XmlContent (OneOf14 a b c d e f g h i j k l m n)
  where
    fromElem cs =
        (choice OneOf14 $ choice TwoOf14 $ choice ThreeOf14 $ choice FourOf14
        $ choice FiveOf14 $ choice SixOf14 $ choice SevenOf14
        $ choice EightOf14 $ choice NineOf14 $ choice TenOf14
        $ choice ElevenOf14 $ choice TwelveOf14 $ choice ThirteenOf14
        $ choice FourteenOf14
        $ (\c->(Nothing,c))) cs
    toElem (OneOf14 x) = toElem x
    toElem (TwoOf14 x) = toElem x
    toElem (ThreeOf14 x) = toElem x
    toElem (FourOf14 x) = toElem x
    toElem (FiveOf14 x) = toElem x
    toElem (SixOf14 x) = toElem x
    toElem (SevenOf14 x) = toElem x
    toElem (EightOf14 x) = toElem x
    toElem (NineOf14 x) = toElem x
    toElem (TenOf14 x) = toElem x
    toElem (ElevenOf14 x) = toElem x
    toElem (TwelveOf14 x) = toElem x
    toElem (ThirteenOf14 x) = toElem x
    toElem (FourteenOf14 x) = toElem x

----
data OneOf15 a b c d e f g h i j k l m n o
    = OneOf15 a | TwoOf15 b | ThreeOf15 c | FourOf15 d | FiveOf15 e
    | SixOf15 f | SevenOf15 g | EightOf15 h | NineOf15 i | TenOf15 j
    | ElevenOf15 k | TwelveOf15 l | ThirteenOf15 m | FourteenOf15 n
    | FifteenOf15 o
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o)
    => XmlContent (OneOf15 a b c d e f g h i j k l m n o)
  where
    fromElem cs =
        (choice OneOf15 $ choice TwoOf15 $ choice ThreeOf15 $ choice FourOf15
        $ choice FiveOf15 $ choice SixOf15 $ choice SevenOf15
        $ choice EightOf15 $ choice NineOf15 $ choice TenOf15
        $ choice ElevenOf15 $ choice TwelveOf15 $ choice ThirteenOf15
        $ choice FourteenOf15 $ choice FifteenOf15
        $ (\c->(Nothing,c))) cs
    toElem (OneOf15 x) = toElem x
    toElem (TwoOf15 x) = toElem x
    toElem (ThreeOf15 x) = toElem x
    toElem (FourOf15 x) = toElem x
    toElem (FiveOf15 x) = toElem x
    toElem (SixOf15 x) = toElem x
    toElem (SevenOf15 x) = toElem x
    toElem (EightOf15 x) = toElem x
    toElem (NineOf15 x) = toElem x
    toElem (TenOf15 x) = toElem x
    toElem (ElevenOf15 x) = toElem x
    toElem (TwelveOf15 x) = toElem x
    toElem (ThirteenOf15 x) = toElem x
    toElem (FourteenOf15 x) = toElem x
    toElem (FifteenOf15 x) = toElem x

----
data OneOf16 a b c d e f g h i j k l m n o p
    = OneOf16 a | TwoOf16 b | ThreeOf16 c | FourOf16 d | FiveOf16 e
    | SixOf16 f | SevenOf16 g | EightOf16 h | NineOf16 i | TenOf16 j
    | ElevenOf16 k | TwelveOf16 l | ThirteenOf16 m | FourteenOf16 n
    | FifteenOf16 o | SixteenOf16 p
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p)
    => XmlContent (OneOf16 a b c d e f g h i j k l m n o p)
  where
    fromElem cs =
        (choice OneOf16 $ choice TwoOf16 $ choice ThreeOf16 $ choice FourOf16
        $ choice FiveOf16 $ choice SixOf16 $ choice SevenOf16
        $ choice EightOf16 $ choice NineOf16 $ choice TenOf16
        $ choice ElevenOf16 $ choice TwelveOf16 $ choice ThirteenOf16
        $ choice FourteenOf16 $ choice FifteenOf16 $ choice SixteenOf16
        $ (\c->(Nothing,c))) cs
    toElem (OneOf16 x) = toElem x
    toElem (TwoOf16 x) = toElem x
    toElem (ThreeOf16 x) = toElem x
    toElem (FourOf16 x) = toElem x
    toElem (FiveOf16 x) = toElem x
    toElem (SixOf16 x) = toElem x
    toElem (SevenOf16 x) = toElem x
    toElem (EightOf16 x) = toElem x
    toElem (NineOf16 x) = toElem x
    toElem (TenOf16 x) = toElem x
    toElem (ElevenOf16 x) = toElem x
    toElem (TwelveOf16 x) = toElem x
    toElem (ThirteenOf16 x) = toElem x
    toElem (FourteenOf16 x) = toElem x
    toElem (FifteenOf16 x) = toElem x
    toElem (SixteenOf16 x) = toElem x

----
data OneOf17 a b c d e f g h i j k l m n o p q
    = OneOf17 a | TwoOf17 b | ThreeOf17 c | FourOf17 d | FiveOf17 e
    | SixOf17 f | SevenOf17 g | EightOf17 h | NineOf17 i | TenOf17 j
    | ElevenOf17 k | TwelveOf17 l | ThirteenOf17 m | FourteenOf17 n
    | FifteenOf17 o | SixteenOf17 p | SeventeenOf17 q
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q)
    => XmlContent (OneOf17 a b c d e f g h i j k l m n o p q)
  where
    fromElem cs =
        (choice OneOf17 $ choice TwoOf17 $ choice ThreeOf17 $ choice FourOf17
        $ choice FiveOf17 $ choice SixOf17 $ choice SevenOf17
        $ choice EightOf17 $ choice NineOf17 $ choice TenOf17
        $ choice ElevenOf17 $ choice TwelveOf17 $ choice ThirteenOf17
        $ choice FourteenOf17 $ choice FifteenOf17 $ choice SixteenOf17
        $ choice SeventeenOf17
        $ (\c->(Nothing,c))) cs
    toElem (OneOf17 x) = toElem x
    toElem (TwoOf17 x) = toElem x
    toElem (ThreeOf17 x) = toElem x
    toElem (FourOf17 x) = toElem x
    toElem (FiveOf17 x) = toElem x
    toElem (SixOf17 x) = toElem x
    toElem (SevenOf17 x) = toElem x
    toElem (EightOf17 x) = toElem x
    toElem (NineOf17 x) = toElem x
    toElem (TenOf17 x) = toElem x
    toElem (ElevenOf17 x) = toElem x
    toElem (TwelveOf17 x) = toElem x
    toElem (ThirteenOf17 x) = toElem x
    toElem (FourteenOf17 x) = toElem x
    toElem (FifteenOf17 x) = toElem x
    toElem (SixteenOf17 x) = toElem x
    toElem (SeventeenOf17 x) = toElem x

----
data OneOf18 a b c d e f g h i j k l m n o p q r
    = OneOf18 a | TwoOf18 b | ThreeOf18 c | FourOf18 d | FiveOf18 e
    | SixOf18 f | SevenOf18 g | EightOf18 h | NineOf18 i | TenOf18 j
    | ElevenOf18 k | TwelveOf18 l | ThirteenOf18 m | FourteenOf18 n
    | FifteenOf18 o | SixteenOf18 p | SeventeenOf18 q | EighteenOf18 r
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r)
    => XmlContent (OneOf18 a b c d e f g h i j k l m n o p q r)
  where
    fromElem cs =
        (choice OneOf18 $ choice TwoOf18 $ choice ThreeOf18 $ choice FourOf18
        $ choice FiveOf18 $ choice SixOf18 $ choice SevenOf18
        $ choice EightOf18 $ choice NineOf18 $ choice TenOf18
        $ choice ElevenOf18 $ choice TwelveOf18 $ choice ThirteenOf18
        $ choice FourteenOf18 $ choice FifteenOf18 $ choice SixteenOf18
        $ choice SeventeenOf18 $ choice EighteenOf18
        $ (\c->(Nothing,c))) cs
    toElem (OneOf18 x) = toElem x
    toElem (TwoOf18 x) = toElem x
    toElem (ThreeOf18 x) = toElem x
    toElem (FourOf18 x) = toElem x
    toElem (FiveOf18 x) = toElem x
    toElem (SixOf18 x) = toElem x
    toElem (SevenOf18 x) = toElem x
    toElem (EightOf18 x) = toElem x
    toElem (NineOf18 x) = toElem x
    toElem (TenOf18 x) = toElem x
    toElem (ElevenOf18 x) = toElem x
    toElem (TwelveOf18 x) = toElem x
    toElem (ThirteenOf18 x) = toElem x
    toElem (FourteenOf18 x) = toElem x
    toElem (FifteenOf18 x) = toElem x
    toElem (SixteenOf18 x) = toElem x
    toElem (SeventeenOf18 x) = toElem x
    toElem (EighteenOf18 x) = toElem x

----
data OneOf19 a b c d e f g h i j k l m n o p q r s
    = OneOf19 a | TwoOf19 b | ThreeOf19 c | FourOf19 d | FiveOf19 e
    | SixOf19 f | SevenOf19 g | EightOf19 h | NineOf19 i | TenOf19 j
    | ElevenOf19 k | TwelveOf19 l | ThirteenOf19 m | FourteenOf19 n
    | FifteenOf19 o | SixteenOf19 p | SeventeenOf19 q | EighteenOf19 r
    | NineteenOf19 s
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s)
    => XmlContent (OneOf19 a b c d e f g h i j k l m n o p q r s)
  where
    fromElem cs =
        (choice OneOf19 $ choice TwoOf19 $ choice ThreeOf19 $ choice FourOf19
        $ choice FiveOf19 $ choice SixOf19 $ choice SevenOf19
        $ choice EightOf19 $ choice NineOf19 $ choice TenOf19
        $ choice ElevenOf19 $ choice TwelveOf19 $ choice ThirteenOf19
        $ choice FourteenOf19 $ choice FifteenOf19 $ choice SixteenOf19
        $ choice SeventeenOf19 $ choice EighteenOf19 $ choice NineteenOf19
        $ (\c->(Nothing,c))) cs
    toElem (OneOf19 x) = toElem x
    toElem (TwoOf19 x) = toElem x
    toElem (ThreeOf19 x) = toElem x
    toElem (FourOf19 x) = toElem x
    toElem (FiveOf19 x) = toElem x
    toElem (SixOf19 x) = toElem x
    toElem (SevenOf19 x) = toElem x
    toElem (EightOf19 x) = toElem x
    toElem (NineOf19 x) = toElem x
    toElem (TenOf19 x) = toElem x
    toElem (ElevenOf19 x) = toElem x
    toElem (TwelveOf19 x) = toElem x
    toElem (ThirteenOf19 x) = toElem x
    toElem (FourteenOf19 x) = toElem x
    toElem (FifteenOf19 x) = toElem x
    toElem (SixteenOf19 x) = toElem x
    toElem (SeventeenOf19 x) = toElem x
    toElem (EighteenOf19 x) = toElem x
    toElem (NineteenOf19 x) = toElem x

----
data OneOf20 a b c d e f g h i j k l m n o p q r s t
    = OneOf20 a | TwoOf20 b | ThreeOf20 c | FourOf20 d | FiveOf20 e
    | SixOf20 f | SevenOf20 g | EightOf20 h | NineOf20 i | TenOf20 j
    | ElevenOf20 k | TwelveOf20 l | ThirteenOf20 m | FourteenOf20 n
    | FifteenOf20 o | SixteenOf20 p | SeventeenOf20 q | EighteenOf20 r
    | NineteenOf20 s | TwentyOf20 t
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t)
    => XmlContent (OneOf20 a b c d e f g h i j k l m n o p q r s t)
  where
    fromElem cs =
        (choice OneOf20 $ choice TwoOf20 $ choice ThreeOf20 $ choice FourOf20
        $ choice FiveOf20 $ choice SixOf20 $ choice SevenOf20
        $ choice EightOf20 $ choice NineOf20 $ choice TenOf20
        $ choice ElevenOf20 $ choice TwelveOf20 $ choice ThirteenOf20
        $ choice FourteenOf20 $ choice FifteenOf20 $ choice SixteenOf20
        $ choice SeventeenOf20 $ choice EighteenOf20 $ choice NineteenOf20
        $ choice TwentyOf20
        $ (\c->(Nothing,c))) cs
    toElem (OneOf20 x) = toElem x
    toElem (TwoOf20 x) = toElem x
    toElem (ThreeOf20 x) = toElem x
    toElem (FourOf20 x) = toElem x
    toElem (FiveOf20 x) = toElem x
    toElem (SixOf20 x) = toElem x
    toElem (SevenOf20 x) = toElem x
    toElem (EightOf20 x) = toElem x
    toElem (NineOf20 x) = toElem x
    toElem (TenOf20 x) = toElem x
    toElem (ElevenOf20 x) = toElem x
    toElem (TwelveOf20 x) = toElem x
    toElem (ThirteenOf20 x) = toElem x
    toElem (FourteenOf20 x) = toElem x
    toElem (FifteenOf20 x) = toElem x
    toElem (SixteenOf20 x) = toElem x
    toElem (SeventeenOf20 x) = toElem x
    toElem (EighteenOf20 x) = toElem x
    toElem (NineteenOf20 x) = toElem x
    toElem (TwentyOf20 x) = toElem x

----
data OneOf21 a b c d e f g h i j k l m n o p q r s t u
    = OneOf21 a | TwoOf21 b | ThreeOf21 c | FourOf21 d | FiveOf21 e
    | SixOf21 f | SevenOf21 g | EightOf21 h | NineOf21 i | TenOf21 j
    | ElevenOf21 k | TwelveOf21 l | ThirteenOf21 m | FourteenOf21 n
    | FifteenOf21 o | SixteenOf21 p | SeventeenOf21 q | EighteenOf21 r
    | NineteenOf21 s | TwentyOf21 t | Choice21Of21 u
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u)
    => XmlContent (OneOf21 a b c d e f g h i j k l m n o p q r s t u)
  where
    fromElem cs =
        (choice OneOf21 $ choice TwoOf21 $ choice ThreeOf21 $ choice FourOf21
        $ choice FiveOf21 $ choice SixOf21 $ choice SevenOf21
        $ choice EightOf21 $ choice NineOf21 $ choice TenOf21
        $ choice ElevenOf21 $ choice TwelveOf21 $ choice ThirteenOf21
        $ choice FourteenOf21 $ choice FifteenOf21 $ choice SixteenOf21
        $ choice SeventeenOf21 $ choice EighteenOf21 $ choice NineteenOf21
        $ choice TwentyOf21 $ choice Choice21Of21
        $ (\c->(Nothing,c))) cs
    toElem (OneOf21 x) = toElem x
    toElem (TwoOf21 x) = toElem x
    toElem (ThreeOf21 x) = toElem x
    toElem (FourOf21 x) = toElem x
    toElem (FiveOf21 x) = toElem x
    toElem (SixOf21 x) = toElem x
    toElem (SevenOf21 x) = toElem x
    toElem (EightOf21 x) = toElem x
    toElem (NineOf21 x) = toElem x
    toElem (TenOf21 x) = toElem x
    toElem (ElevenOf21 x) = toElem x
    toElem (TwelveOf21 x) = toElem x
    toElem (ThirteenOf21 x) = toElem x
    toElem (FourteenOf21 x) = toElem x
    toElem (FifteenOf21 x) = toElem x
    toElem (SixteenOf21 x) = toElem x
    toElem (SeventeenOf21 x) = toElem x
    toElem (EighteenOf21 x) = toElem x
    toElem (NineteenOf21 x) = toElem x
    toElem (TwentyOf21 x) = toElem x
    toElem (Choice21Of21 x) = toElem x

----
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
data OneOf23 a b c d e f g h i j k l m n o p q r s t u v w
    = OneOf23 a | TwoOf23 b | ThreeOf23 c | FourOf23 d | FiveOf23 e
    | SixOf23 f | SevenOf23 g | EightOf23 h | NineOf23 i | TenOf23 j
    | ElevenOf23 k | TwelveOf23 l | ThirteenOf23 m | FourteenOf23 n
    | FifteenOf23 o | SixteenOf23 p | SeventeenOf23 q | EighteenOf23 r
    | NineteenOf23 s | TwentyOf23 t | Choice21Of23 u | Choice22Of23 v
    | Choice23Of23 w
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w)
    => XmlContent (OneOf23 a b c d e f g h i j k l m n o p q r s t u v w)
  where
    fromElem cs =
        (choice OneOf23 $ choice TwoOf23 $ choice ThreeOf23 $ choice FourOf23
        $ choice FiveOf23 $ choice SixOf23 $ choice SevenOf23
        $ choice EightOf23 $ choice NineOf23 $ choice TenOf23
        $ choice ElevenOf23 $ choice TwelveOf23 $ choice ThirteenOf23
        $ choice FourteenOf23 $ choice FifteenOf23 $ choice SixteenOf23
        $ choice SeventeenOf23 $ choice EighteenOf23 $ choice NineteenOf23
        $ choice TwentyOf23 $ choice Choice21Of23 $ choice Choice22Of23
        $ choice Choice23Of23
        $ (\c->(Nothing,c))) cs
    toElem (OneOf23 x) = toElem x
    toElem (TwoOf23 x) = toElem x
    toElem (ThreeOf23 x) = toElem x
    toElem (FourOf23 x) = toElem x
    toElem (FiveOf23 x) = toElem x
    toElem (SixOf23 x) = toElem x
    toElem (SevenOf23 x) = toElem x
    toElem (EightOf23 x) = toElem x
    toElem (NineOf23 x) = toElem x
    toElem (TenOf23 x) = toElem x
    toElem (ElevenOf23 x) = toElem x
    toElem (TwelveOf23 x) = toElem x
    toElem (ThirteenOf23 x) = toElem x
    toElem (FourteenOf23 x) = toElem x
    toElem (FifteenOf23 x) = toElem x
    toElem (SixteenOf23 x) = toElem x
    toElem (SeventeenOf23 x) = toElem x
    toElem (EighteenOf23 x) = toElem x
    toElem (NineteenOf23 x) = toElem x
    toElem (TwentyOf23 x) = toElem x
    toElem (Choice21Of23 x) = toElem x
    toElem (Choice22Of23 x) = toElem x
    toElem (Choice23Of23 x) = toElem x

----
data OneOf24 a b c d e f g h i j k l m n o p q r s t u v w x
    = OneOf24 a | TwoOf24 b | ThreeOf24 c | FourOf24 d | FiveOf24 e
    | SixOf24 f | SevenOf24 g | EightOf24 h | NineOf24 i | TenOf24 j
    | ElevenOf24 k | TwelveOf24 l | ThirteenOf24 m | FourteenOf24 n
    | FifteenOf24 o | SixteenOf24 p | SeventeenOf24 q | EighteenOf24 r
    | NineteenOf24 s | TwentyOf24 t | Choice21Of24 u | Choice22Of24 v
    | Choice23Of24 w | Choice24Of24 x
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x)
    => XmlContent (OneOf24 a b c d e f g h i j k l m n o p q r s t u v w x)
  where
    fromElem cs =
        (choice OneOf24 $ choice TwoOf24 $ choice ThreeOf24 $ choice FourOf24
        $ choice FiveOf24 $ choice SixOf24 $ choice SevenOf24
        $ choice EightOf24 $ choice NineOf24 $ choice TenOf24
        $ choice ElevenOf24 $ choice TwelveOf24 $ choice ThirteenOf24
        $ choice FourteenOf24 $ choice FifteenOf24 $ choice SixteenOf24
        $ choice SeventeenOf24 $ choice EighteenOf24 $ choice NineteenOf24
        $ choice TwentyOf24 $ choice Choice21Of24 $ choice Choice22Of24
        $ choice Choice23Of24 $ choice Choice24Of24
        $ (\c->(Nothing,c))) cs
    toElem (OneOf24 x) = toElem x
    toElem (TwoOf24 x) = toElem x
    toElem (ThreeOf24 x) = toElem x
    toElem (FourOf24 x) = toElem x
    toElem (FiveOf24 x) = toElem x
    toElem (SixOf24 x) = toElem x
    toElem (SevenOf24 x) = toElem x
    toElem (EightOf24 x) = toElem x
    toElem (NineOf24 x) = toElem x
    toElem (TenOf24 x) = toElem x
    toElem (ElevenOf24 x) = toElem x
    toElem (TwelveOf24 x) = toElem x
    toElem (ThirteenOf24 x) = toElem x
    toElem (FourteenOf24 x) = toElem x
    toElem (FifteenOf24 x) = toElem x
    toElem (SixteenOf24 x) = toElem x
    toElem (SeventeenOf24 x) = toElem x
    toElem (EighteenOf24 x) = toElem x
    toElem (NineteenOf24 x) = toElem x
    toElem (TwentyOf24 x) = toElem x
    toElem (Choice21Of24 x) = toElem x
    toElem (Choice22Of24 x) = toElem x
    toElem (Choice23Of24 x) = toElem x
    toElem (Choice24Of24 x) = toElem x

----
data OneOf25 a b c d e f g h i j k l m n o p q r s t u v w x y
    = OneOf25 a | TwoOf25 b | ThreeOf25 c | FourOf25 d | FiveOf25 e
    | SixOf25 f | SevenOf25 g | EightOf25 h | NineOf25 i | TenOf25 j
    | ElevenOf25 k | TwelveOf25 l | ThirteenOf25 m | FourteenOf25 n
    | FifteenOf25 o | SixteenOf25 p | SeventeenOf25 q | EighteenOf25 r
    | NineteenOf25 s | TwentyOf25 t | Choice21Of25 u | Choice22Of25 v
    | Choice23Of25 w | Choice24Of25 x | Choice25Of25 y
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y)
    => XmlContent (OneOf25 a b c d e f g h i j k l m n o p q r s t u v w x y)
  where
    fromElem cs =
        (choice OneOf25 $ choice TwoOf25 $ choice ThreeOf25 $ choice FourOf25
        $ choice FiveOf25 $ choice SixOf25 $ choice SevenOf25
        $ choice EightOf25 $ choice NineOf25 $ choice TenOf25
        $ choice ElevenOf25 $ choice TwelveOf25 $ choice ThirteenOf25
        $ choice FourteenOf25 $ choice FifteenOf25 $ choice SixteenOf25
        $ choice SeventeenOf25 $ choice EighteenOf25 $ choice NineteenOf25
        $ choice TwentyOf25 $ choice Choice21Of25 $ choice Choice22Of25
        $ choice Choice23Of25 $ choice Choice24Of25 $ choice Choice25Of25
        $ (\c->(Nothing,c))) cs
    toElem (OneOf25 x) = toElem x
    toElem (TwoOf25 x) = toElem x
    toElem (ThreeOf25 x) = toElem x
    toElem (FourOf25 x) = toElem x
    toElem (FiveOf25 x) = toElem x
    toElem (SixOf25 x) = toElem x
    toElem (SevenOf25 x) = toElem x
    toElem (EightOf25 x) = toElem x
    toElem (NineOf25 x) = toElem x
    toElem (TenOf25 x) = toElem x
    toElem (ElevenOf25 x) = toElem x
    toElem (TwelveOf25 x) = toElem x
    toElem (ThirteenOf25 x) = toElem x
    toElem (FourteenOf25 x) = toElem x
    toElem (FifteenOf25 x) = toElem x
    toElem (SixteenOf25 x) = toElem x
    toElem (SeventeenOf25 x) = toElem x
    toElem (EighteenOf25 x) = toElem x
    toElem (NineteenOf25 x) = toElem x
    toElem (TwentyOf25 x) = toElem x
    toElem (Choice21Of25 x) = toElem x
    toElem (Choice22Of25 x) = toElem x
    toElem (Choice23Of25 x) = toElem x
    toElem (Choice24Of25 x) = toElem x
    toElem (Choice25Of25 x) = toElem x

----
data OneOf26 a b c d e f g h i j k l m n o p q r s t u v w x y z
    = OneOf26 a | TwoOf26 b | ThreeOf26 c | FourOf26 d | FiveOf26 e
    | SixOf26 f | SevenOf26 g | EightOf26 h | NineOf26 i | TenOf26 j
    | ElevenOf26 k | TwelveOf26 l | ThirteenOf26 m | FourteenOf26 n
    | FifteenOf26 o | SixteenOf26 p | SeventeenOf26 q | EighteenOf26 r
    | NineteenOf26 s | TwentyOf26 t | Choice21Of26 u | Choice22Of26 v
    | Choice23Of26 w | Choice24Of26 x | Choice25Of26 y | Choice26Of26 z
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z)
    => XmlContent (OneOf26 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z)
  where
    fromElem cs =
        (choice OneOf26 $ choice TwoOf26 $ choice ThreeOf26 $ choice FourOf26
        $ choice FiveOf26 $ choice SixOf26 $ choice SevenOf26
        $ choice EightOf26 $ choice NineOf26 $ choice TenOf26
        $ choice ElevenOf26 $ choice TwelveOf26 $ choice ThirteenOf26
        $ choice FourteenOf26 $ choice FifteenOf26 $ choice SixteenOf26
        $ choice SeventeenOf26 $ choice EighteenOf26 $ choice NineteenOf26
        $ choice TwentyOf26 $ choice Choice21Of26 $ choice Choice22Of26
        $ choice Choice23Of26 $ choice Choice24Of26 $ choice Choice25Of26
        $ choice Choice26Of26
        $ (\c->(Nothing,c))) cs
    toElem (OneOf26 x) = toElem x
    toElem (TwoOf26 x) = toElem x
    toElem (ThreeOf26 x) = toElem x
    toElem (FourOf26 x) = toElem x
    toElem (FiveOf26 x) = toElem x
    toElem (SixOf26 x) = toElem x
    toElem (SevenOf26 x) = toElem x
    toElem (EightOf26 x) = toElem x
    toElem (NineOf26 x) = toElem x
    toElem (TenOf26 x) = toElem x
    toElem (ElevenOf26 x) = toElem x
    toElem (TwelveOf26 x) = toElem x
    toElem (ThirteenOf26 x) = toElem x
    toElem (FourteenOf26 x) = toElem x
    toElem (FifteenOf26 x) = toElem x
    toElem (SixteenOf26 x) = toElem x
    toElem (SeventeenOf26 x) = toElem x
    toElem (EighteenOf26 x) = toElem x
    toElem (NineteenOf26 x) = toElem x
    toElem (TwentyOf26 x) = toElem x
    toElem (Choice21Of26 x) = toElem x
    toElem (Choice22Of26 x) = toElem x
    toElem (Choice23Of26 x) = toElem x
    toElem (Choice24Of26 x) = toElem x
    toElem (Choice25Of26 x) = toElem x
    toElem (Choice26Of26 x) = toElem x

----
data OneOf27 a b c d e f g h i j k l m n o p q r s t u v w x y z aa
    = OneOf27 a | TwoOf27 b | ThreeOf27 c | FourOf27 d | FiveOf27 e
    | SixOf27 f | SevenOf27 g | EightOf27 h | NineOf27 i | TenOf27 j
    | ElevenOf27 k | TwelveOf27 l | ThirteenOf27 m | FourteenOf27 n
    | FifteenOf27 o | SixteenOf27 p | SeventeenOf27 q | EighteenOf27 r
    | NineteenOf27 s | TwentyOf27 t | Choice21Of27 u | Choice22Of27 v
    | Choice23Of27 w | Choice24Of27 x | Choice25Of27 y | Choice26Of27 z
    | Choice27Of27 aa
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa)
    => XmlContent (OneOf27 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa)
  where
    fromElem cs =
        (choice OneOf27 $ choice TwoOf27 $ choice ThreeOf27 $ choice FourOf27
        $ choice FiveOf27 $ choice SixOf27 $ choice SevenOf27
        $ choice EightOf27 $ choice NineOf27 $ choice TenOf27
        $ choice ElevenOf27 $ choice TwelveOf27 $ choice ThirteenOf27
        $ choice FourteenOf27 $ choice FifteenOf27 $ choice SixteenOf27
        $ choice SeventeenOf27 $ choice EighteenOf27 $ choice NineteenOf27
        $ choice TwentyOf27 $ choice Choice21Of27 $ choice Choice22Of27
        $ choice Choice23Of27 $ choice Choice24Of27 $ choice Choice25Of27
        $ choice Choice26Of27 $ choice Choice27Of27
        $ (\c->(Nothing,c))) cs
    toElem (OneOf27 x) = toElem x
    toElem (TwoOf27 x) = toElem x
    toElem (ThreeOf27 x) = toElem x
    toElem (FourOf27 x) = toElem x
    toElem (FiveOf27 x) = toElem x
    toElem (SixOf27 x) = toElem x
    toElem (SevenOf27 x) = toElem x
    toElem (EightOf27 x) = toElem x
    toElem (NineOf27 x) = toElem x
    toElem (TenOf27 x) = toElem x
    toElem (ElevenOf27 x) = toElem x
    toElem (TwelveOf27 x) = toElem x
    toElem (ThirteenOf27 x) = toElem x
    toElem (FourteenOf27 x) = toElem x
    toElem (FifteenOf27 x) = toElem x
    toElem (SixteenOf27 x) = toElem x
    toElem (SeventeenOf27 x) = toElem x
    toElem (EighteenOf27 x) = toElem x
    toElem (NineteenOf27 x) = toElem x
    toElem (TwentyOf27 x) = toElem x
    toElem (Choice21Of27 x) = toElem x
    toElem (Choice22Of27 x) = toElem x
    toElem (Choice23Of27 x) = toElem x
    toElem (Choice24Of27 x) = toElem x
    toElem (Choice25Of27 x) = toElem x
    toElem (Choice26Of27 x) = toElem x
    toElem (Choice27Of27 x) = toElem x

----
data OneOf28 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab
    = OneOf28 a | TwoOf28 b | ThreeOf28 c | FourOf28 d | FiveOf28 e
    | SixOf28 f | SevenOf28 g | EightOf28 h | NineOf28 i | TenOf28 j
    | ElevenOf28 k | TwelveOf28 l | ThirteenOf28 m | FourteenOf28 n
    | FifteenOf28 o | SixteenOf28 p | SeventeenOf28 q | EighteenOf28 r
    | NineteenOf28 s | TwentyOf28 t | Choice21Of28 u | Choice22Of28 v
    | Choice23Of28 w | Choice24Of28 x | Choice25Of28 y | Choice26Of28 z
    | Choice27Of28 aa | Choice28Of28 ab
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab)
    => XmlContent (OneOf28 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab)
  where
    fromElem cs =
        (choice OneOf28 $ choice TwoOf28 $ choice ThreeOf28 $ choice FourOf28
        $ choice FiveOf28 $ choice SixOf28 $ choice SevenOf28
        $ choice EightOf28 $ choice NineOf28 $ choice TenOf28
        $ choice ElevenOf28 $ choice TwelveOf28 $ choice ThirteenOf28
        $ choice FourteenOf28 $ choice FifteenOf28 $ choice SixteenOf28
        $ choice SeventeenOf28 $ choice EighteenOf28 $ choice NineteenOf28
        $ choice TwentyOf28 $ choice Choice21Of28 $ choice Choice22Of28
        $ choice Choice23Of28 $ choice Choice24Of28 $ choice Choice25Of28
        $ choice Choice26Of28 $ choice Choice27Of28 $ choice Choice28Of28
        $ (\c->(Nothing,c))) cs
    toElem (OneOf28 x) = toElem x
    toElem (TwoOf28 x) = toElem x
    toElem (ThreeOf28 x) = toElem x
    toElem (FourOf28 x) = toElem x
    toElem (FiveOf28 x) = toElem x
    toElem (SixOf28 x) = toElem x
    toElem (SevenOf28 x) = toElem x
    toElem (EightOf28 x) = toElem x
    toElem (NineOf28 x) = toElem x
    toElem (TenOf28 x) = toElem x
    toElem (ElevenOf28 x) = toElem x
    toElem (TwelveOf28 x) = toElem x
    toElem (ThirteenOf28 x) = toElem x
    toElem (FourteenOf28 x) = toElem x
    toElem (FifteenOf28 x) = toElem x
    toElem (SixteenOf28 x) = toElem x
    toElem (SeventeenOf28 x) = toElem x
    toElem (EighteenOf28 x) = toElem x
    toElem (NineteenOf28 x) = toElem x
    toElem (TwentyOf28 x) = toElem x
    toElem (Choice21Of28 x) = toElem x
    toElem (Choice22Of28 x) = toElem x
    toElem (Choice23Of28 x) = toElem x
    toElem (Choice24Of28 x) = toElem x
    toElem (Choice25Of28 x) = toElem x
    toElem (Choice26Of28 x) = toElem x
    toElem (Choice27Of28 x) = toElem x
    toElem (Choice28Of28 x) = toElem x

----
data OneOf29 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac
    = OneOf29 a | TwoOf29 b | ThreeOf29 c | FourOf29 d | FiveOf29 e
    | SixOf29 f | SevenOf29 g | EightOf29 h | NineOf29 i | TenOf29 j
    | ElevenOf29 k | TwelveOf29 l | ThirteenOf29 m | FourteenOf29 n
    | FifteenOf29 o | SixteenOf29 p | SeventeenOf29 q | EighteenOf29 r
    | NineteenOf29 s | TwentyOf29 t | Choice21Of29 u | Choice22Of29 v
    | Choice23Of29 w | Choice24Of29 x | Choice25Of29 y | Choice26Of29 z
    | Choice27Of29 aa | Choice28Of29 ab | Choice29Of29 ac
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac)
    => XmlContent (OneOf29 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac)
  where
    fromElem cs =
        (choice OneOf29 $ choice TwoOf29 $ choice ThreeOf29 $ choice FourOf29
        $ choice FiveOf29 $ choice SixOf29 $ choice SevenOf29
        $ choice EightOf29 $ choice NineOf29 $ choice TenOf29
        $ choice ElevenOf29 $ choice TwelveOf29 $ choice ThirteenOf29
        $ choice FourteenOf29 $ choice FifteenOf29 $ choice SixteenOf29
        $ choice SeventeenOf29 $ choice EighteenOf29 $ choice NineteenOf29
        $ choice TwentyOf29 $ choice Choice21Of29 $ choice Choice22Of29
        $ choice Choice23Of29 $ choice Choice24Of29 $ choice Choice25Of29
        $ choice Choice26Of29 $ choice Choice27Of29 $ choice Choice28Of29
        $ choice Choice29Of29
        $ (\c->(Nothing,c))) cs
    toElem (OneOf29 x) = toElem x
    toElem (TwoOf29 x) = toElem x
    toElem (ThreeOf29 x) = toElem x
    toElem (FourOf29 x) = toElem x
    toElem (FiveOf29 x) = toElem x
    toElem (SixOf29 x) = toElem x
    toElem (SevenOf29 x) = toElem x
    toElem (EightOf29 x) = toElem x
    toElem (NineOf29 x) = toElem x
    toElem (TenOf29 x) = toElem x
    toElem (ElevenOf29 x) = toElem x
    toElem (TwelveOf29 x) = toElem x
    toElem (ThirteenOf29 x) = toElem x
    toElem (FourteenOf29 x) = toElem x
    toElem (FifteenOf29 x) = toElem x
    toElem (SixteenOf29 x) = toElem x
    toElem (SeventeenOf29 x) = toElem x
    toElem (EighteenOf29 x) = toElem x
    toElem (NineteenOf29 x) = toElem x
    toElem (TwentyOf29 x) = toElem x
    toElem (Choice21Of29 x) = toElem x
    toElem (Choice22Of29 x) = toElem x
    toElem (Choice23Of29 x) = toElem x
    toElem (Choice24Of29 x) = toElem x
    toElem (Choice25Of29 x) = toElem x
    toElem (Choice26Of29 x) = toElem x
    toElem (Choice27Of29 x) = toElem x
    toElem (Choice28Of29 x) = toElem x
    toElem (Choice29Of29 x) = toElem x

----
data OneOf30 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
    = OneOf30 a | TwoOf30 b | ThreeOf30 c | FourOf30 d | FiveOf30 e
    | SixOf30 f | SevenOf30 g | EightOf30 h | NineOf30 i | TenOf30 j
    | ElevenOf30 k | TwelveOf30 l | ThirteenOf30 m | FourteenOf30 n
    | FifteenOf30 o | SixteenOf30 p | SeventeenOf30 q | EighteenOf30 r
    | NineteenOf30 s | TwentyOf30 t | Choice21Of30 u | Choice22Of30 v
    | Choice23Of30 w | Choice24Of30 x | Choice25Of30 y | Choice26Of30 z
    | Choice27Of30 aa | Choice28Of30 ab | Choice29Of30 ac | Choice30Of30 ad
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad)
    => XmlContent (OneOf30 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad)
  where
    fromElem cs =
        (choice OneOf30 $ choice TwoOf30 $ choice ThreeOf30 $ choice FourOf30
        $ choice FiveOf30 $ choice SixOf30 $ choice SevenOf30
        $ choice EightOf30 $ choice NineOf30 $ choice TenOf30
        $ choice ElevenOf30 $ choice TwelveOf30 $ choice ThirteenOf30
        $ choice FourteenOf30 $ choice FifteenOf30 $ choice SixteenOf30
        $ choice SeventeenOf30 $ choice EighteenOf30 $ choice NineteenOf30
        $ choice TwentyOf30 $ choice Choice21Of30 $ choice Choice22Of30
        $ choice Choice23Of30 $ choice Choice24Of30 $ choice Choice25Of30
        $ choice Choice26Of30 $ choice Choice27Of30 $ choice Choice28Of30
        $ choice Choice29Of30 $ choice Choice30Of30
        $ (\c->(Nothing,c))) cs
    toElem (OneOf30 x) = toElem x
    toElem (TwoOf30 x) = toElem x
    toElem (ThreeOf30 x) = toElem x
    toElem (FourOf30 x) = toElem x
    toElem (FiveOf30 x) = toElem x
    toElem (SixOf30 x) = toElem x
    toElem (SevenOf30 x) = toElem x
    toElem (EightOf30 x) = toElem x
    toElem (NineOf30 x) = toElem x
    toElem (TenOf30 x) = toElem x
    toElem (ElevenOf30 x) = toElem x
    toElem (TwelveOf30 x) = toElem x
    toElem (ThirteenOf30 x) = toElem x
    toElem (FourteenOf30 x) = toElem x
    toElem (FifteenOf30 x) = toElem x
    toElem (SixteenOf30 x) = toElem x
    toElem (SeventeenOf30 x) = toElem x
    toElem (EighteenOf30 x) = toElem x
    toElem (NineteenOf30 x) = toElem x
    toElem (TwentyOf30 x) = toElem x
    toElem (Choice21Of30 x) = toElem x
    toElem (Choice22Of30 x) = toElem x
    toElem (Choice23Of30 x) = toElem x
    toElem (Choice24Of30 x) = toElem x
    toElem (Choice25Of30 x) = toElem x
    toElem (Choice26Of30 x) = toElem x
    toElem (Choice27Of30 x) = toElem x
    toElem (Choice28Of30 x) = toElem x
    toElem (Choice29Of30 x) = toElem x
    toElem (Choice30Of30 x) = toElem x

----
data OneOf31 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae
    = OneOf31 a | TwoOf31 b | ThreeOf31 c | FourOf31 d | FiveOf31 e
    | SixOf31 f | SevenOf31 g | EightOf31 h | NineOf31 i | TenOf31 j
    | ElevenOf31 k | TwelveOf31 l | ThirteenOf31 m | FourteenOf31 n
    | FifteenOf31 o | SixteenOf31 p | SeventeenOf31 q | EighteenOf31 r
    | NineteenOf31 s | TwentyOf31 t | Choice21Of31 u | Choice22Of31 v
    | Choice23Of31 w | Choice24Of31 x | Choice25Of31 y | Choice26Of31 z
    | Choice27Of31 aa | Choice28Of31 ab | Choice29Of31 ac | Choice30Of31 ad
    | Choice31Of31 ae
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae)
    => XmlContent (OneOf31 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae)
  where
    fromElem cs =
        (choice OneOf31 $ choice TwoOf31 $ choice ThreeOf31 $ choice FourOf31
        $ choice FiveOf31 $ choice SixOf31 $ choice SevenOf31
        $ choice EightOf31 $ choice NineOf31 $ choice TenOf31
        $ choice ElevenOf31 $ choice TwelveOf31 $ choice ThirteenOf31
        $ choice FourteenOf31 $ choice FifteenOf31 $ choice SixteenOf31
        $ choice SeventeenOf31 $ choice EighteenOf31 $ choice NineteenOf31
        $ choice TwentyOf31 $ choice Choice21Of31 $ choice Choice22Of31
        $ choice Choice23Of31 $ choice Choice24Of31 $ choice Choice25Of31
        $ choice Choice26Of31 $ choice Choice27Of31 $ choice Choice28Of31
        $ choice Choice29Of31 $ choice Choice30Of31 $ choice Choice31Of31
        $ (\c->(Nothing,c))) cs
    toElem (OneOf31 x) = toElem x
    toElem (TwoOf31 x) = toElem x
    toElem (ThreeOf31 x) = toElem x
    toElem (FourOf31 x) = toElem x
    toElem (FiveOf31 x) = toElem x
    toElem (SixOf31 x) = toElem x
    toElem (SevenOf31 x) = toElem x
    toElem (EightOf31 x) = toElem x
    toElem (NineOf31 x) = toElem x
    toElem (TenOf31 x) = toElem x
    toElem (ElevenOf31 x) = toElem x
    toElem (TwelveOf31 x) = toElem x
    toElem (ThirteenOf31 x) = toElem x
    toElem (FourteenOf31 x) = toElem x
    toElem (FifteenOf31 x) = toElem x
    toElem (SixteenOf31 x) = toElem x
    toElem (SeventeenOf31 x) = toElem x
    toElem (EighteenOf31 x) = toElem x
    toElem (NineteenOf31 x) = toElem x
    toElem (TwentyOf31 x) = toElem x
    toElem (Choice21Of31 x) = toElem x
    toElem (Choice22Of31 x) = toElem x
    toElem (Choice23Of31 x) = toElem x
    toElem (Choice24Of31 x) = toElem x
    toElem (Choice25Of31 x) = toElem x
    toElem (Choice26Of31 x) = toElem x
    toElem (Choice27Of31 x) = toElem x
    toElem (Choice28Of31 x) = toElem x
    toElem (Choice29Of31 x) = toElem x
    toElem (Choice30Of31 x) = toElem x
    toElem (Choice31Of31 x) = toElem x

----
data OneOf32 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af
    = OneOf32 a | TwoOf32 b | ThreeOf32 c | FourOf32 d | FiveOf32 e
    | SixOf32 f | SevenOf32 g | EightOf32 h | NineOf32 i | TenOf32 j
    | ElevenOf32 k | TwelveOf32 l | ThirteenOf32 m | FourteenOf32 n
    | FifteenOf32 o | SixteenOf32 p | SeventeenOf32 q | EighteenOf32 r
    | NineteenOf32 s | TwentyOf32 t | Choice21Of32 u | Choice22Of32 v
    | Choice23Of32 w | Choice24Of32 x | Choice25Of32 y | Choice26Of32 z
    | Choice27Of32 aa | Choice28Of32 ab | Choice29Of32 ac | Choice30Of32 ad
    | Choice31Of32 ae | Choice32Of32 af
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af)
    => XmlContent (OneOf32 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af)
  where
    fromElem cs =
        (choice OneOf32 $ choice TwoOf32 $ choice ThreeOf32 $ choice FourOf32
        $ choice FiveOf32 $ choice SixOf32 $ choice SevenOf32
        $ choice EightOf32 $ choice NineOf32 $ choice TenOf32
        $ choice ElevenOf32 $ choice TwelveOf32 $ choice ThirteenOf32
        $ choice FourteenOf32 $ choice FifteenOf32 $ choice SixteenOf32
        $ choice SeventeenOf32 $ choice EighteenOf32 $ choice NineteenOf32
        $ choice TwentyOf32 $ choice Choice21Of32 $ choice Choice22Of32
        $ choice Choice23Of32 $ choice Choice24Of32 $ choice Choice25Of32
        $ choice Choice26Of32 $ choice Choice27Of32 $ choice Choice28Of32
        $ choice Choice29Of32 $ choice Choice30Of32 $ choice Choice31Of32
        $ choice Choice32Of32
        $ (\c->(Nothing,c))) cs
    toElem (OneOf32 x) = toElem x
    toElem (TwoOf32 x) = toElem x
    toElem (ThreeOf32 x) = toElem x
    toElem (FourOf32 x) = toElem x
    toElem (FiveOf32 x) = toElem x
    toElem (SixOf32 x) = toElem x
    toElem (SevenOf32 x) = toElem x
    toElem (EightOf32 x) = toElem x
    toElem (NineOf32 x) = toElem x
    toElem (TenOf32 x) = toElem x
    toElem (ElevenOf32 x) = toElem x
    toElem (TwelveOf32 x) = toElem x
    toElem (ThirteenOf32 x) = toElem x
    toElem (FourteenOf32 x) = toElem x
    toElem (FifteenOf32 x) = toElem x
    toElem (SixteenOf32 x) = toElem x
    toElem (SeventeenOf32 x) = toElem x
    toElem (EighteenOf32 x) = toElem x
    toElem (NineteenOf32 x) = toElem x
    toElem (TwentyOf32 x) = toElem x
    toElem (Choice21Of32 x) = toElem x
    toElem (Choice22Of32 x) = toElem x
    toElem (Choice23Of32 x) = toElem x
    toElem (Choice24Of32 x) = toElem x
    toElem (Choice25Of32 x) = toElem x
    toElem (Choice26Of32 x) = toElem x
    toElem (Choice27Of32 x) = toElem x
    toElem (Choice28Of32 x) = toElem x
    toElem (Choice29Of32 x) = toElem x
    toElem (Choice30Of32 x) = toElem x
    toElem (Choice31Of32 x) = toElem x
    toElem (Choice32Of32 x) = toElem x

----
data OneOf33 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag
    = OneOf33 a | TwoOf33 b | ThreeOf33 c | FourOf33 d | FiveOf33 e
    | SixOf33 f | SevenOf33 g | EightOf33 h | NineOf33 i | TenOf33 j
    | ElevenOf33 k | TwelveOf33 l | ThirteenOf33 m | FourteenOf33 n
    | FifteenOf33 o | SixteenOf33 p | SeventeenOf33 q | EighteenOf33 r
    | NineteenOf33 s | TwentyOf33 t | Choice21Of33 u | Choice22Of33 v
    | Choice23Of33 w | Choice24Of33 x | Choice25Of33 y | Choice26Of33 z
    | Choice27Of33 aa | Choice28Of33 ab | Choice29Of33 ac | Choice30Of33 ad
    | Choice31Of33 ae | Choice32Of33 af | Choice33Of33 ag
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag)
    => XmlContent (OneOf33 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag)
  where
    fromElem cs =
        (choice OneOf33 $ choice TwoOf33 $ choice ThreeOf33 $ choice FourOf33
        $ choice FiveOf33 $ choice SixOf33 $ choice SevenOf33
        $ choice EightOf33 $ choice NineOf33 $ choice TenOf33
        $ choice ElevenOf33 $ choice TwelveOf33 $ choice ThirteenOf33
        $ choice FourteenOf33 $ choice FifteenOf33 $ choice SixteenOf33
        $ choice SeventeenOf33 $ choice EighteenOf33 $ choice NineteenOf33
        $ choice TwentyOf33 $ choice Choice21Of33 $ choice Choice22Of33
        $ choice Choice23Of33 $ choice Choice24Of33 $ choice Choice25Of33
        $ choice Choice26Of33 $ choice Choice27Of33 $ choice Choice28Of33
        $ choice Choice29Of33 $ choice Choice30Of33 $ choice Choice31Of33
        $ choice Choice32Of33 $ choice Choice33Of33
        $ (\c->(Nothing,c))) cs
    toElem (OneOf33 x) = toElem x
    toElem (TwoOf33 x) = toElem x
    toElem (ThreeOf33 x) = toElem x
    toElem (FourOf33 x) = toElem x
    toElem (FiveOf33 x) = toElem x
    toElem (SixOf33 x) = toElem x
    toElem (SevenOf33 x) = toElem x
    toElem (EightOf33 x) = toElem x
    toElem (NineOf33 x) = toElem x
    toElem (TenOf33 x) = toElem x
    toElem (ElevenOf33 x) = toElem x
    toElem (TwelveOf33 x) = toElem x
    toElem (ThirteenOf33 x) = toElem x
    toElem (FourteenOf33 x) = toElem x
    toElem (FifteenOf33 x) = toElem x
    toElem (SixteenOf33 x) = toElem x
    toElem (SeventeenOf33 x) = toElem x
    toElem (EighteenOf33 x) = toElem x
    toElem (NineteenOf33 x) = toElem x
    toElem (TwentyOf33 x) = toElem x
    toElem (Choice21Of33 x) = toElem x
    toElem (Choice22Of33 x) = toElem x
    toElem (Choice23Of33 x) = toElem x
    toElem (Choice24Of33 x) = toElem x
    toElem (Choice25Of33 x) = toElem x
    toElem (Choice26Of33 x) = toElem x
    toElem (Choice27Of33 x) = toElem x
    toElem (Choice28Of33 x) = toElem x
    toElem (Choice29Of33 x) = toElem x
    toElem (Choice30Of33 x) = toElem x
    toElem (Choice31Of33 x) = toElem x
    toElem (Choice32Of33 x) = toElem x
    toElem (Choice33Of33 x) = toElem x

----
data OneOf34 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah
    = OneOf34 a | TwoOf34 b | ThreeOf34 c | FourOf34 d | FiveOf34 e
    | SixOf34 f | SevenOf34 g | EightOf34 h | NineOf34 i | TenOf34 j
    | ElevenOf34 k | TwelveOf34 l | ThirteenOf34 m | FourteenOf34 n
    | FifteenOf34 o | SixteenOf34 p | SeventeenOf34 q | EighteenOf34 r
    | NineteenOf34 s | TwentyOf34 t | Choice21Of34 u | Choice22Of34 v
    | Choice23Of34 w | Choice24Of34 x | Choice25Of34 y | Choice26Of34 z
    | Choice27Of34 aa | Choice28Of34 ab | Choice29Of34 ac | Choice30Of34 ad
    | Choice31Of34 ae | Choice32Of34 af | Choice33Of34 ag | Choice34Of34 ah
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah)
    => XmlContent (OneOf34 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah)
  where
    fromElem cs =
        (choice OneOf34 $ choice TwoOf34 $ choice ThreeOf34 $ choice FourOf34
        $ choice FiveOf34 $ choice SixOf34 $ choice SevenOf34
        $ choice EightOf34 $ choice NineOf34 $ choice TenOf34
        $ choice ElevenOf34 $ choice TwelveOf34 $ choice ThirteenOf34
        $ choice FourteenOf34 $ choice FifteenOf34 $ choice SixteenOf34
        $ choice SeventeenOf34 $ choice EighteenOf34 $ choice NineteenOf34
        $ choice TwentyOf34 $ choice Choice21Of34 $ choice Choice22Of34
        $ choice Choice23Of34 $ choice Choice24Of34 $ choice Choice25Of34
        $ choice Choice26Of34 $ choice Choice27Of34 $ choice Choice28Of34
        $ choice Choice29Of34 $ choice Choice30Of34 $ choice Choice31Of34
        $ choice Choice32Of34 $ choice Choice33Of34 $ choice Choice34Of34
        $ (\c->(Nothing,c))) cs
    toElem (OneOf34 x) = toElem x
    toElem (TwoOf34 x) = toElem x
    toElem (ThreeOf34 x) = toElem x
    toElem (FourOf34 x) = toElem x
    toElem (FiveOf34 x) = toElem x
    toElem (SixOf34 x) = toElem x
    toElem (SevenOf34 x) = toElem x
    toElem (EightOf34 x) = toElem x
    toElem (NineOf34 x) = toElem x
    toElem (TenOf34 x) = toElem x
    toElem (ElevenOf34 x) = toElem x
    toElem (TwelveOf34 x) = toElem x
    toElem (ThirteenOf34 x) = toElem x
    toElem (FourteenOf34 x) = toElem x
    toElem (FifteenOf34 x) = toElem x
    toElem (SixteenOf34 x) = toElem x
    toElem (SeventeenOf34 x) = toElem x
    toElem (EighteenOf34 x) = toElem x
    toElem (NineteenOf34 x) = toElem x
    toElem (TwentyOf34 x) = toElem x
    toElem (Choice21Of34 x) = toElem x
    toElem (Choice22Of34 x) = toElem x
    toElem (Choice23Of34 x) = toElem x
    toElem (Choice24Of34 x) = toElem x
    toElem (Choice25Of34 x) = toElem x
    toElem (Choice26Of34 x) = toElem x
    toElem (Choice27Of34 x) = toElem x
    toElem (Choice28Of34 x) = toElem x
    toElem (Choice29Of34 x) = toElem x
    toElem (Choice30Of34 x) = toElem x
    toElem (Choice31Of34 x) = toElem x
    toElem (Choice32Of34 x) = toElem x
    toElem (Choice33Of34 x) = toElem x
    toElem (Choice34Of34 x) = toElem x

----
data OneOf35 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai
    = OneOf35 a | TwoOf35 b | ThreeOf35 c | FourOf35 d | FiveOf35 e
    | SixOf35 f | SevenOf35 g | EightOf35 h | NineOf35 i | TenOf35 j
    | ElevenOf35 k | TwelveOf35 l | ThirteenOf35 m | FourteenOf35 n
    | FifteenOf35 o | SixteenOf35 p | SeventeenOf35 q | EighteenOf35 r
    | NineteenOf35 s | TwentyOf35 t | Choice21Of35 u | Choice22Of35 v
    | Choice23Of35 w | Choice24Of35 x | Choice25Of35 y | Choice26Of35 z
    | Choice27Of35 aa | Choice28Of35 ab | Choice29Of35 ac | Choice30Of35 ad
    | Choice31Of35 ae | Choice32Of35 af | Choice33Of35 ag | Choice34Of35 ah
    | Choice35Of35 ai
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai)
    => XmlContent (OneOf35 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai)
  where
    fromElem cs =
        (choice OneOf35 $ choice TwoOf35 $ choice ThreeOf35 $ choice FourOf35
        $ choice FiveOf35 $ choice SixOf35 $ choice SevenOf35
        $ choice EightOf35 $ choice NineOf35 $ choice TenOf35
        $ choice ElevenOf35 $ choice TwelveOf35 $ choice ThirteenOf35
        $ choice FourteenOf35 $ choice FifteenOf35 $ choice SixteenOf35
        $ choice SeventeenOf35 $ choice EighteenOf35 $ choice NineteenOf35
        $ choice TwentyOf35 $ choice Choice21Of35 $ choice Choice22Of35
        $ choice Choice23Of35 $ choice Choice24Of35 $ choice Choice25Of35
        $ choice Choice26Of35 $ choice Choice27Of35 $ choice Choice28Of35
        $ choice Choice29Of35 $ choice Choice30Of35 $ choice Choice31Of35
        $ choice Choice32Of35 $ choice Choice33Of35 $ choice Choice34Of35
        $ choice Choice35Of35
        $ (\c->(Nothing,c))) cs
    toElem (OneOf35 x) = toElem x
    toElem (TwoOf35 x) = toElem x
    toElem (ThreeOf35 x) = toElem x
    toElem (FourOf35 x) = toElem x
    toElem (FiveOf35 x) = toElem x
    toElem (SixOf35 x) = toElem x
    toElem (SevenOf35 x) = toElem x
    toElem (EightOf35 x) = toElem x
    toElem (NineOf35 x) = toElem x
    toElem (TenOf35 x) = toElem x
    toElem (ElevenOf35 x) = toElem x
    toElem (TwelveOf35 x) = toElem x
    toElem (ThirteenOf35 x) = toElem x
    toElem (FourteenOf35 x) = toElem x
    toElem (FifteenOf35 x) = toElem x
    toElem (SixteenOf35 x) = toElem x
    toElem (SeventeenOf35 x) = toElem x
    toElem (EighteenOf35 x) = toElem x
    toElem (NineteenOf35 x) = toElem x
    toElem (TwentyOf35 x) = toElem x
    toElem (Choice21Of35 x) = toElem x
    toElem (Choice22Of35 x) = toElem x
    toElem (Choice23Of35 x) = toElem x
    toElem (Choice24Of35 x) = toElem x
    toElem (Choice25Of35 x) = toElem x
    toElem (Choice26Of35 x) = toElem x
    toElem (Choice27Of35 x) = toElem x
    toElem (Choice28Of35 x) = toElem x
    toElem (Choice29Of35 x) = toElem x
    toElem (Choice30Of35 x) = toElem x
    toElem (Choice31Of35 x) = toElem x
    toElem (Choice32Of35 x) = toElem x
    toElem (Choice33Of35 x) = toElem x
    toElem (Choice34Of35 x) = toElem x
    toElem (Choice35Of35 x) = toElem x

----
data OneOf36 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj
    = OneOf36 a | TwoOf36 b | ThreeOf36 c | FourOf36 d | FiveOf36 e
    | SixOf36 f | SevenOf36 g | EightOf36 h | NineOf36 i | TenOf36 j
    | ElevenOf36 k | TwelveOf36 l | ThirteenOf36 m | FourteenOf36 n
    | FifteenOf36 o | SixteenOf36 p | SeventeenOf36 q | EighteenOf36 r
    | NineteenOf36 s | TwentyOf36 t | Choice21Of36 u | Choice22Of36 v
    | Choice23Of36 w | Choice24Of36 x | Choice25Of36 y | Choice26Of36 z
    | Choice27Of36 aa | Choice28Of36 ab | Choice29Of36 ac | Choice30Of36 ad
    | Choice31Of36 ae | Choice32Of36 af | Choice33Of36 ag | Choice34Of36 ah
    | Choice35Of36 ai | Choice36Of36 aj
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj)
    => XmlContent (OneOf36 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj)
  where
    fromElem cs =
        (choice OneOf36 $ choice TwoOf36 $ choice ThreeOf36 $ choice FourOf36
        $ choice FiveOf36 $ choice SixOf36 $ choice SevenOf36
        $ choice EightOf36 $ choice NineOf36 $ choice TenOf36
        $ choice ElevenOf36 $ choice TwelveOf36 $ choice ThirteenOf36
        $ choice FourteenOf36 $ choice FifteenOf36 $ choice SixteenOf36
        $ choice SeventeenOf36 $ choice EighteenOf36 $ choice NineteenOf36
        $ choice TwentyOf36 $ choice Choice21Of36 $ choice Choice22Of36
        $ choice Choice23Of36 $ choice Choice24Of36 $ choice Choice25Of36
        $ choice Choice26Of36 $ choice Choice27Of36 $ choice Choice28Of36
        $ choice Choice29Of36 $ choice Choice30Of36 $ choice Choice31Of36
        $ choice Choice32Of36 $ choice Choice33Of36 $ choice Choice34Of36
        $ choice Choice35Of36 $ choice Choice36Of36
        $ (\c->(Nothing,c))) cs
    toElem (OneOf36 x) = toElem x
    toElem (TwoOf36 x) = toElem x
    toElem (ThreeOf36 x) = toElem x
    toElem (FourOf36 x) = toElem x
    toElem (FiveOf36 x) = toElem x
    toElem (SixOf36 x) = toElem x
    toElem (SevenOf36 x) = toElem x
    toElem (EightOf36 x) = toElem x
    toElem (NineOf36 x) = toElem x
    toElem (TenOf36 x) = toElem x
    toElem (ElevenOf36 x) = toElem x
    toElem (TwelveOf36 x) = toElem x
    toElem (ThirteenOf36 x) = toElem x
    toElem (FourteenOf36 x) = toElem x
    toElem (FifteenOf36 x) = toElem x
    toElem (SixteenOf36 x) = toElem x
    toElem (SeventeenOf36 x) = toElem x
    toElem (EighteenOf36 x) = toElem x
    toElem (NineteenOf36 x) = toElem x
    toElem (TwentyOf36 x) = toElem x
    toElem (Choice21Of36 x) = toElem x
    toElem (Choice22Of36 x) = toElem x
    toElem (Choice23Of36 x) = toElem x
    toElem (Choice24Of36 x) = toElem x
    toElem (Choice25Of36 x) = toElem x
    toElem (Choice26Of36 x) = toElem x
    toElem (Choice27Of36 x) = toElem x
    toElem (Choice28Of36 x) = toElem x
    toElem (Choice29Of36 x) = toElem x
    toElem (Choice30Of36 x) = toElem x
    toElem (Choice31Of36 x) = toElem x
    toElem (Choice32Of36 x) = toElem x
    toElem (Choice33Of36 x) = toElem x
    toElem (Choice34Of36 x) = toElem x
    toElem (Choice35Of36 x) = toElem x
    toElem (Choice36Of36 x) = toElem x

----
data OneOf37 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak
    = OneOf37 a | TwoOf37 b | ThreeOf37 c | FourOf37 d | FiveOf37 e
    | SixOf37 f | SevenOf37 g | EightOf37 h | NineOf37 i | TenOf37 j
    | ElevenOf37 k | TwelveOf37 l | ThirteenOf37 m | FourteenOf37 n
    | FifteenOf37 o | SixteenOf37 p | SeventeenOf37 q | EighteenOf37 r
    | NineteenOf37 s | TwentyOf37 t | Choice21Of37 u | Choice22Of37 v
    | Choice23Of37 w | Choice24Of37 x | Choice25Of37 y | Choice26Of37 z
    | Choice27Of37 aa | Choice28Of37 ab | Choice29Of37 ac | Choice30Of37 ad
    | Choice31Of37 ae | Choice32Of37 af | Choice33Of37 ag | Choice34Of37 ah
    | Choice35Of37 ai | Choice36Of37 aj | Choice37Of37 ak
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak)
    => XmlContent (OneOf37 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak)
  where
    fromElem cs =
        (choice OneOf37 $ choice TwoOf37 $ choice ThreeOf37 $ choice FourOf37
        $ choice FiveOf37 $ choice SixOf37 $ choice SevenOf37
        $ choice EightOf37 $ choice NineOf37 $ choice TenOf37
        $ choice ElevenOf37 $ choice TwelveOf37 $ choice ThirteenOf37
        $ choice FourteenOf37 $ choice FifteenOf37 $ choice SixteenOf37
        $ choice SeventeenOf37 $ choice EighteenOf37 $ choice NineteenOf37
        $ choice TwentyOf37 $ choice Choice21Of37 $ choice Choice22Of37
        $ choice Choice23Of37 $ choice Choice24Of37 $ choice Choice25Of37
        $ choice Choice26Of37 $ choice Choice27Of37 $ choice Choice28Of37
        $ choice Choice29Of37 $ choice Choice30Of37 $ choice Choice31Of37
        $ choice Choice32Of37 $ choice Choice33Of37 $ choice Choice34Of37
        $ choice Choice35Of37 $ choice Choice36Of37 $ choice Choice37Of37
        $ (\c->(Nothing,c))) cs
    toElem (OneOf37 x) = toElem x
    toElem (TwoOf37 x) = toElem x
    toElem (ThreeOf37 x) = toElem x
    toElem (FourOf37 x) = toElem x
    toElem (FiveOf37 x) = toElem x
    toElem (SixOf37 x) = toElem x
    toElem (SevenOf37 x) = toElem x
    toElem (EightOf37 x) = toElem x
    toElem (NineOf37 x) = toElem x
    toElem (TenOf37 x) = toElem x
    toElem (ElevenOf37 x) = toElem x
    toElem (TwelveOf37 x) = toElem x
    toElem (ThirteenOf37 x) = toElem x
    toElem (FourteenOf37 x) = toElem x
    toElem (FifteenOf37 x) = toElem x
    toElem (SixteenOf37 x) = toElem x
    toElem (SeventeenOf37 x) = toElem x
    toElem (EighteenOf37 x) = toElem x
    toElem (NineteenOf37 x) = toElem x
    toElem (TwentyOf37 x) = toElem x
    toElem (Choice21Of37 x) = toElem x
    toElem (Choice22Of37 x) = toElem x
    toElem (Choice23Of37 x) = toElem x
    toElem (Choice24Of37 x) = toElem x
    toElem (Choice25Of37 x) = toElem x
    toElem (Choice26Of37 x) = toElem x
    toElem (Choice27Of37 x) = toElem x
    toElem (Choice28Of37 x) = toElem x
    toElem (Choice29Of37 x) = toElem x
    toElem (Choice30Of37 x) = toElem x
    toElem (Choice31Of37 x) = toElem x
    toElem (Choice32Of37 x) = toElem x
    toElem (Choice33Of37 x) = toElem x
    toElem (Choice34Of37 x) = toElem x
    toElem (Choice35Of37 x) = toElem x
    toElem (Choice36Of37 x) = toElem x
    toElem (Choice37Of37 x) = toElem x

----
data OneOf38 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al
    = OneOf38 a | TwoOf38 b | ThreeOf38 c | FourOf38 d | FiveOf38 e
    | SixOf38 f | SevenOf38 g | EightOf38 h | NineOf38 i | TenOf38 j
    | ElevenOf38 k | TwelveOf38 l | ThirteenOf38 m | FourteenOf38 n
    | FifteenOf38 o | SixteenOf38 p | SeventeenOf38 q | EighteenOf38 r
    | NineteenOf38 s | TwentyOf38 t | Choice21Of38 u | Choice22Of38 v
    | Choice23Of38 w | Choice24Of38 x | Choice25Of38 y | Choice26Of38 z
    | Choice27Of38 aa | Choice28Of38 ab | Choice29Of38 ac | Choice30Of38 ad
    | Choice31Of38 ae | Choice32Of38 af | Choice33Of38 ag | Choice34Of38 ah
    | Choice35Of38 ai | Choice36Of38 aj | Choice37Of38 ak | Choice38Of38 al
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al)
    => XmlContent (OneOf38 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al)
  where
    fromElem cs =
        (choice OneOf38 $ choice TwoOf38 $ choice ThreeOf38 $ choice FourOf38
        $ choice FiveOf38 $ choice SixOf38 $ choice SevenOf38
        $ choice EightOf38 $ choice NineOf38 $ choice TenOf38
        $ choice ElevenOf38 $ choice TwelveOf38 $ choice ThirteenOf38
        $ choice FourteenOf38 $ choice FifteenOf38 $ choice SixteenOf38
        $ choice SeventeenOf38 $ choice EighteenOf38 $ choice NineteenOf38
        $ choice TwentyOf38 $ choice Choice21Of38 $ choice Choice22Of38
        $ choice Choice23Of38 $ choice Choice24Of38 $ choice Choice25Of38
        $ choice Choice26Of38 $ choice Choice27Of38 $ choice Choice28Of38
        $ choice Choice29Of38 $ choice Choice30Of38 $ choice Choice31Of38
        $ choice Choice32Of38 $ choice Choice33Of38 $ choice Choice34Of38
        $ choice Choice35Of38 $ choice Choice36Of38 $ choice Choice37Of38
        $ choice Choice38Of38
        $ (\c->(Nothing,c))) cs
    toElem (OneOf38 x) = toElem x
    toElem (TwoOf38 x) = toElem x
    toElem (ThreeOf38 x) = toElem x
    toElem (FourOf38 x) = toElem x
    toElem (FiveOf38 x) = toElem x
    toElem (SixOf38 x) = toElem x
    toElem (SevenOf38 x) = toElem x
    toElem (EightOf38 x) = toElem x
    toElem (NineOf38 x) = toElem x
    toElem (TenOf38 x) = toElem x
    toElem (ElevenOf38 x) = toElem x
    toElem (TwelveOf38 x) = toElem x
    toElem (ThirteenOf38 x) = toElem x
    toElem (FourteenOf38 x) = toElem x
    toElem (FifteenOf38 x) = toElem x
    toElem (SixteenOf38 x) = toElem x
    toElem (SeventeenOf38 x) = toElem x
    toElem (EighteenOf38 x) = toElem x
    toElem (NineteenOf38 x) = toElem x
    toElem (TwentyOf38 x) = toElem x
    toElem (Choice21Of38 x) = toElem x
    toElem (Choice22Of38 x) = toElem x
    toElem (Choice23Of38 x) = toElem x
    toElem (Choice24Of38 x) = toElem x
    toElem (Choice25Of38 x) = toElem x
    toElem (Choice26Of38 x) = toElem x
    toElem (Choice27Of38 x) = toElem x
    toElem (Choice28Of38 x) = toElem x
    toElem (Choice29Of38 x) = toElem x
    toElem (Choice30Of38 x) = toElem x
    toElem (Choice31Of38 x) = toElem x
    toElem (Choice32Of38 x) = toElem x
    toElem (Choice33Of38 x) = toElem x
    toElem (Choice34Of38 x) = toElem x
    toElem (Choice35Of38 x) = toElem x
    toElem (Choice36Of38 x) = toElem x
    toElem (Choice37Of38 x) = toElem x
    toElem (Choice38Of38 x) = toElem x

----
data OneOf39 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am
    = OneOf39 a | TwoOf39 b | ThreeOf39 c | FourOf39 d | FiveOf39 e
    | SixOf39 f | SevenOf39 g | EightOf39 h | NineOf39 i | TenOf39 j
    | ElevenOf39 k | TwelveOf39 l | ThirteenOf39 m | FourteenOf39 n
    | FifteenOf39 o | SixteenOf39 p | SeventeenOf39 q | EighteenOf39 r
    | NineteenOf39 s | TwentyOf39 t | Choice21Of39 u | Choice22Of39 v
    | Choice23Of39 w | Choice24Of39 x | Choice25Of39 y | Choice26Of39 z
    | Choice27Of39 aa | Choice28Of39 ab | Choice29Of39 ac | Choice30Of39 ad
    | Choice31Of39 ae | Choice32Of39 af | Choice33Of39 ag | Choice34Of39 ah
    | Choice35Of39 ai | Choice36Of39 aj | Choice37Of39 ak | Choice38Of39 al
    | Choice39Of39 am
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am)
    => XmlContent (OneOf39 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am)
  where
    fromElem cs =
        (choice OneOf39 $ choice TwoOf39 $ choice ThreeOf39 $ choice FourOf39
        $ choice FiveOf39 $ choice SixOf39 $ choice SevenOf39
        $ choice EightOf39 $ choice NineOf39 $ choice TenOf39
        $ choice ElevenOf39 $ choice TwelveOf39 $ choice ThirteenOf39
        $ choice FourteenOf39 $ choice FifteenOf39 $ choice SixteenOf39
        $ choice SeventeenOf39 $ choice EighteenOf39 $ choice NineteenOf39
        $ choice TwentyOf39 $ choice Choice21Of39 $ choice Choice22Of39
        $ choice Choice23Of39 $ choice Choice24Of39 $ choice Choice25Of39
        $ choice Choice26Of39 $ choice Choice27Of39 $ choice Choice28Of39
        $ choice Choice29Of39 $ choice Choice30Of39 $ choice Choice31Of39
        $ choice Choice32Of39 $ choice Choice33Of39 $ choice Choice34Of39
        $ choice Choice35Of39 $ choice Choice36Of39 $ choice Choice37Of39
        $ choice Choice38Of39 $ choice Choice39Of39
        $ (\c->(Nothing,c))) cs
    toElem (OneOf39 x) = toElem x
    toElem (TwoOf39 x) = toElem x
    toElem (ThreeOf39 x) = toElem x
    toElem (FourOf39 x) = toElem x
    toElem (FiveOf39 x) = toElem x
    toElem (SixOf39 x) = toElem x
    toElem (SevenOf39 x) = toElem x
    toElem (EightOf39 x) = toElem x
    toElem (NineOf39 x) = toElem x
    toElem (TenOf39 x) = toElem x
    toElem (ElevenOf39 x) = toElem x
    toElem (TwelveOf39 x) = toElem x
    toElem (ThirteenOf39 x) = toElem x
    toElem (FourteenOf39 x) = toElem x
    toElem (FifteenOf39 x) = toElem x
    toElem (SixteenOf39 x) = toElem x
    toElem (SeventeenOf39 x) = toElem x
    toElem (EighteenOf39 x) = toElem x
    toElem (NineteenOf39 x) = toElem x
    toElem (TwentyOf39 x) = toElem x
    toElem (Choice21Of39 x) = toElem x
    toElem (Choice22Of39 x) = toElem x
    toElem (Choice23Of39 x) = toElem x
    toElem (Choice24Of39 x) = toElem x
    toElem (Choice25Of39 x) = toElem x
    toElem (Choice26Of39 x) = toElem x
    toElem (Choice27Of39 x) = toElem x
    toElem (Choice28Of39 x) = toElem x
    toElem (Choice29Of39 x) = toElem x
    toElem (Choice30Of39 x) = toElem x
    toElem (Choice31Of39 x) = toElem x
    toElem (Choice32Of39 x) = toElem x
    toElem (Choice33Of39 x) = toElem x
    toElem (Choice34Of39 x) = toElem x
    toElem (Choice35Of39 x) = toElem x
    toElem (Choice36Of39 x) = toElem x
    toElem (Choice37Of39 x) = toElem x
    toElem (Choice38Of39 x) = toElem x
    toElem (Choice39Of39 x) = toElem x

----
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
data OneOf41 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao
    = OneOf41 a | TwoOf41 b | ThreeOf41 c | FourOf41 d | FiveOf41 e
    | SixOf41 f | SevenOf41 g | EightOf41 h | NineOf41 i | TenOf41 j
    | ElevenOf41 k | TwelveOf41 l | ThirteenOf41 m | FourteenOf41 n
    | FifteenOf41 o | SixteenOf41 p | SeventeenOf41 q | EighteenOf41 r
    | NineteenOf41 s | TwentyOf41 t | Choice21Of41 u | Choice22Of41 v
    | Choice23Of41 w | Choice24Of41 x | Choice25Of41 y | Choice26Of41 z
    | Choice27Of41 aa | Choice28Of41 ab | Choice29Of41 ac | Choice30Of41 ad
    | Choice31Of41 ae | Choice32Of41 af | Choice33Of41 ag | Choice34Of41 ah
    | Choice35Of41 ai | Choice36Of41 aj | Choice37Of41 ak | Choice38Of41 al
    | Choice39Of41 am | Choice40Of41 an | Choice41Of41 ao
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao)
    => XmlContent (OneOf41 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao)
  where
    fromElem cs =
        (choice OneOf41 $ choice TwoOf41 $ choice ThreeOf41 $ choice FourOf41
        $ choice FiveOf41 $ choice SixOf41 $ choice SevenOf41
        $ choice EightOf41 $ choice NineOf41 $ choice TenOf41
        $ choice ElevenOf41 $ choice TwelveOf41 $ choice ThirteenOf41
        $ choice FourteenOf41 $ choice FifteenOf41 $ choice SixteenOf41
        $ choice SeventeenOf41 $ choice EighteenOf41 $ choice NineteenOf41
        $ choice TwentyOf41 $ choice Choice21Of41 $ choice Choice22Of41
        $ choice Choice23Of41 $ choice Choice24Of41 $ choice Choice25Of41
        $ choice Choice26Of41 $ choice Choice27Of41 $ choice Choice28Of41
        $ choice Choice29Of41 $ choice Choice30Of41 $ choice Choice31Of41
        $ choice Choice32Of41 $ choice Choice33Of41 $ choice Choice34Of41
        $ choice Choice35Of41 $ choice Choice36Of41 $ choice Choice37Of41
        $ choice Choice38Of41 $ choice Choice39Of41 $ choice Choice40Of41
        $ choice Choice41Of41
        $ (\c->(Nothing,c))) cs
    toElem (OneOf41 x) = toElem x
    toElem (TwoOf41 x) = toElem x
    toElem (ThreeOf41 x) = toElem x
    toElem (FourOf41 x) = toElem x
    toElem (FiveOf41 x) = toElem x
    toElem (SixOf41 x) = toElem x
    toElem (SevenOf41 x) = toElem x
    toElem (EightOf41 x) = toElem x
    toElem (NineOf41 x) = toElem x
    toElem (TenOf41 x) = toElem x
    toElem (ElevenOf41 x) = toElem x
    toElem (TwelveOf41 x) = toElem x
    toElem (ThirteenOf41 x) = toElem x
    toElem (FourteenOf41 x) = toElem x
    toElem (FifteenOf41 x) = toElem x
    toElem (SixteenOf41 x) = toElem x
    toElem (SeventeenOf41 x) = toElem x
    toElem (EighteenOf41 x) = toElem x
    toElem (NineteenOf41 x) = toElem x
    toElem (TwentyOf41 x) = toElem x
    toElem (Choice21Of41 x) = toElem x
    toElem (Choice22Of41 x) = toElem x
    toElem (Choice23Of41 x) = toElem x
    toElem (Choice24Of41 x) = toElem x
    toElem (Choice25Of41 x) = toElem x
    toElem (Choice26Of41 x) = toElem x
    toElem (Choice27Of41 x) = toElem x
    toElem (Choice28Of41 x) = toElem x
    toElem (Choice29Of41 x) = toElem x
    toElem (Choice30Of41 x) = toElem x
    toElem (Choice31Of41 x) = toElem x
    toElem (Choice32Of41 x) = toElem x
    toElem (Choice33Of41 x) = toElem x
    toElem (Choice34Of41 x) = toElem x
    toElem (Choice35Of41 x) = toElem x
    toElem (Choice36Of41 x) = toElem x
    toElem (Choice37Of41 x) = toElem x
    toElem (Choice38Of41 x) = toElem x
    toElem (Choice39Of41 x) = toElem x
    toElem (Choice40Of41 x) = toElem x
    toElem (Choice41Of41 x) = toElem x

----
data OneOf42 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap
    = OneOf42 a | TwoOf42 b | ThreeOf42 c | FourOf42 d | FiveOf42 e
    | SixOf42 f | SevenOf42 g | EightOf42 h | NineOf42 i | TenOf42 j
    | ElevenOf42 k | TwelveOf42 l | ThirteenOf42 m | FourteenOf42 n
    | FifteenOf42 o | SixteenOf42 p | SeventeenOf42 q | EighteenOf42 r
    | NineteenOf42 s | TwentyOf42 t | Choice21Of42 u | Choice22Of42 v
    | Choice23Of42 w | Choice24Of42 x | Choice25Of42 y | Choice26Of42 z
    | Choice27Of42 aa | Choice28Of42 ab | Choice29Of42 ac | Choice30Of42 ad
    | Choice31Of42 ae | Choice32Of42 af | Choice33Of42 ag | Choice34Of42 ah
    | Choice35Of42 ai | Choice36Of42 aj | Choice37Of42 ak | Choice38Of42 al
    | Choice39Of42 am | Choice40Of42 an | Choice41Of42 ao | Choice42Of42 ap
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap)
    => XmlContent (OneOf42 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap)
  where
    fromElem cs =
        (choice OneOf42 $ choice TwoOf42 $ choice ThreeOf42 $ choice FourOf42
        $ choice FiveOf42 $ choice SixOf42 $ choice SevenOf42
        $ choice EightOf42 $ choice NineOf42 $ choice TenOf42
        $ choice ElevenOf42 $ choice TwelveOf42 $ choice ThirteenOf42
        $ choice FourteenOf42 $ choice FifteenOf42 $ choice SixteenOf42
        $ choice SeventeenOf42 $ choice EighteenOf42 $ choice NineteenOf42
        $ choice TwentyOf42 $ choice Choice21Of42 $ choice Choice22Of42
        $ choice Choice23Of42 $ choice Choice24Of42 $ choice Choice25Of42
        $ choice Choice26Of42 $ choice Choice27Of42 $ choice Choice28Of42
        $ choice Choice29Of42 $ choice Choice30Of42 $ choice Choice31Of42
        $ choice Choice32Of42 $ choice Choice33Of42 $ choice Choice34Of42
        $ choice Choice35Of42 $ choice Choice36Of42 $ choice Choice37Of42
        $ choice Choice38Of42 $ choice Choice39Of42 $ choice Choice40Of42
        $ choice Choice41Of42 $ choice Choice42Of42
        $ (\c->(Nothing,c))) cs
    toElem (OneOf42 x) = toElem x
    toElem (TwoOf42 x) = toElem x
    toElem (ThreeOf42 x) = toElem x
    toElem (FourOf42 x) = toElem x
    toElem (FiveOf42 x) = toElem x
    toElem (SixOf42 x) = toElem x
    toElem (SevenOf42 x) = toElem x
    toElem (EightOf42 x) = toElem x
    toElem (NineOf42 x) = toElem x
    toElem (TenOf42 x) = toElem x
    toElem (ElevenOf42 x) = toElem x
    toElem (TwelveOf42 x) = toElem x
    toElem (ThirteenOf42 x) = toElem x
    toElem (FourteenOf42 x) = toElem x
    toElem (FifteenOf42 x) = toElem x
    toElem (SixteenOf42 x) = toElem x
    toElem (SeventeenOf42 x) = toElem x
    toElem (EighteenOf42 x) = toElem x
    toElem (NineteenOf42 x) = toElem x
    toElem (TwentyOf42 x) = toElem x
    toElem (Choice21Of42 x) = toElem x
    toElem (Choice22Of42 x) = toElem x
    toElem (Choice23Of42 x) = toElem x
    toElem (Choice24Of42 x) = toElem x
    toElem (Choice25Of42 x) = toElem x
    toElem (Choice26Of42 x) = toElem x
    toElem (Choice27Of42 x) = toElem x
    toElem (Choice28Of42 x) = toElem x
    toElem (Choice29Of42 x) = toElem x
    toElem (Choice30Of42 x) = toElem x
    toElem (Choice31Of42 x) = toElem x
    toElem (Choice32Of42 x) = toElem x
    toElem (Choice33Of42 x) = toElem x
    toElem (Choice34Of42 x) = toElem x
    toElem (Choice35Of42 x) = toElem x
    toElem (Choice36Of42 x) = toElem x
    toElem (Choice37Of42 x) = toElem x
    toElem (Choice38Of42 x) = toElem x
    toElem (Choice39Of42 x) = toElem x
    toElem (Choice40Of42 x) = toElem x
    toElem (Choice41Of42 x) = toElem x
    toElem (Choice42Of42 x) = toElem x

----
data OneOf43 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq
    = OneOf43 a | TwoOf43 b | ThreeOf43 c | FourOf43 d | FiveOf43 e
    | SixOf43 f | SevenOf43 g | EightOf43 h | NineOf43 i | TenOf43 j
    | ElevenOf43 k | TwelveOf43 l | ThirteenOf43 m | FourteenOf43 n
    | FifteenOf43 o | SixteenOf43 p | SeventeenOf43 q | EighteenOf43 r
    | NineteenOf43 s | TwentyOf43 t | Choice21Of43 u | Choice22Of43 v
    | Choice23Of43 w | Choice24Of43 x | Choice25Of43 y | Choice26Of43 z
    | Choice27Of43 aa | Choice28Of43 ab | Choice29Of43 ac | Choice30Of43 ad
    | Choice31Of43 ae | Choice32Of43 af | Choice33Of43 ag | Choice34Of43 ah
    | Choice35Of43 ai | Choice36Of43 aj | Choice37Of43 ak | Choice38Of43 al
    | Choice39Of43 am | Choice40Of43 an | Choice41Of43 ao | Choice42Of43 ap
    | Choice43Of43 aq
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq)
    => XmlContent (OneOf43 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq)
  where
    fromElem cs =
        (choice OneOf43 $ choice TwoOf43 $ choice ThreeOf43 $ choice FourOf43
        $ choice FiveOf43 $ choice SixOf43 $ choice SevenOf43
        $ choice EightOf43 $ choice NineOf43 $ choice TenOf43
        $ choice ElevenOf43 $ choice TwelveOf43 $ choice ThirteenOf43
        $ choice FourteenOf43 $ choice FifteenOf43 $ choice SixteenOf43
        $ choice SeventeenOf43 $ choice EighteenOf43 $ choice NineteenOf43
        $ choice TwentyOf43 $ choice Choice21Of43 $ choice Choice22Of43
        $ choice Choice23Of43 $ choice Choice24Of43 $ choice Choice25Of43
        $ choice Choice26Of43 $ choice Choice27Of43 $ choice Choice28Of43
        $ choice Choice29Of43 $ choice Choice30Of43 $ choice Choice31Of43
        $ choice Choice32Of43 $ choice Choice33Of43 $ choice Choice34Of43
        $ choice Choice35Of43 $ choice Choice36Of43 $ choice Choice37Of43
        $ choice Choice38Of43 $ choice Choice39Of43 $ choice Choice40Of43
        $ choice Choice41Of43 $ choice Choice42Of43 $ choice Choice43Of43
        $ (\c->(Nothing,c))) cs
    toElem (OneOf43 x) = toElem x
    toElem (TwoOf43 x) = toElem x
    toElem (ThreeOf43 x) = toElem x
    toElem (FourOf43 x) = toElem x
    toElem (FiveOf43 x) = toElem x
    toElem (SixOf43 x) = toElem x
    toElem (SevenOf43 x) = toElem x
    toElem (EightOf43 x) = toElem x
    toElem (NineOf43 x) = toElem x
    toElem (TenOf43 x) = toElem x
    toElem (ElevenOf43 x) = toElem x
    toElem (TwelveOf43 x) = toElem x
    toElem (ThirteenOf43 x) = toElem x
    toElem (FourteenOf43 x) = toElem x
    toElem (FifteenOf43 x) = toElem x
    toElem (SixteenOf43 x) = toElem x
    toElem (SeventeenOf43 x) = toElem x
    toElem (EighteenOf43 x) = toElem x
    toElem (NineteenOf43 x) = toElem x
    toElem (TwentyOf43 x) = toElem x
    toElem (Choice21Of43 x) = toElem x
    toElem (Choice22Of43 x) = toElem x
    toElem (Choice23Of43 x) = toElem x
    toElem (Choice24Of43 x) = toElem x
    toElem (Choice25Of43 x) = toElem x
    toElem (Choice26Of43 x) = toElem x
    toElem (Choice27Of43 x) = toElem x
    toElem (Choice28Of43 x) = toElem x
    toElem (Choice29Of43 x) = toElem x
    toElem (Choice30Of43 x) = toElem x
    toElem (Choice31Of43 x) = toElem x
    toElem (Choice32Of43 x) = toElem x
    toElem (Choice33Of43 x) = toElem x
    toElem (Choice34Of43 x) = toElem x
    toElem (Choice35Of43 x) = toElem x
    toElem (Choice36Of43 x) = toElem x
    toElem (Choice37Of43 x) = toElem x
    toElem (Choice38Of43 x) = toElem x
    toElem (Choice39Of43 x) = toElem x
    toElem (Choice40Of43 x) = toElem x
    toElem (Choice41Of43 x) = toElem x
    toElem (Choice42Of43 x) = toElem x
    toElem (Choice43Of43 x) = toElem x

----
data OneOf44 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar
    = OneOf44 a | TwoOf44 b | ThreeOf44 c | FourOf44 d | FiveOf44 e
    | SixOf44 f | SevenOf44 g | EightOf44 h | NineOf44 i | TenOf44 j
    | ElevenOf44 k | TwelveOf44 l | ThirteenOf44 m | FourteenOf44 n
    | FifteenOf44 o | SixteenOf44 p | SeventeenOf44 q | EighteenOf44 r
    | NineteenOf44 s | TwentyOf44 t | Choice21Of44 u | Choice22Of44 v
    | Choice23Of44 w | Choice24Of44 x | Choice25Of44 y | Choice26Of44 z
    | Choice27Of44 aa | Choice28Of44 ab | Choice29Of44 ac | Choice30Of44 ad
    | Choice31Of44 ae | Choice32Of44 af | Choice33Of44 ag | Choice34Of44 ah
    | Choice35Of44 ai | Choice36Of44 aj | Choice37Of44 ak | Choice38Of44 al
    | Choice39Of44 am | Choice40Of44 an | Choice41Of44 ao | Choice42Of44 ap
    | Choice43Of44 aq | Choice44Of44 ar
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar)
    => XmlContent (OneOf44 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar)
  where
    fromElem cs =
        (choice OneOf44 $ choice TwoOf44 $ choice ThreeOf44 $ choice FourOf44
        $ choice FiveOf44 $ choice SixOf44 $ choice SevenOf44
        $ choice EightOf44 $ choice NineOf44 $ choice TenOf44
        $ choice ElevenOf44 $ choice TwelveOf44 $ choice ThirteenOf44
        $ choice FourteenOf44 $ choice FifteenOf44 $ choice SixteenOf44
        $ choice SeventeenOf44 $ choice EighteenOf44 $ choice NineteenOf44
        $ choice TwentyOf44 $ choice Choice21Of44 $ choice Choice22Of44
        $ choice Choice23Of44 $ choice Choice24Of44 $ choice Choice25Of44
        $ choice Choice26Of44 $ choice Choice27Of44 $ choice Choice28Of44
        $ choice Choice29Of44 $ choice Choice30Of44 $ choice Choice31Of44
        $ choice Choice32Of44 $ choice Choice33Of44 $ choice Choice34Of44
        $ choice Choice35Of44 $ choice Choice36Of44 $ choice Choice37Of44
        $ choice Choice38Of44 $ choice Choice39Of44 $ choice Choice40Of44
        $ choice Choice41Of44 $ choice Choice42Of44 $ choice Choice43Of44
        $ choice Choice44Of44
        $ (\c->(Nothing,c))) cs
    toElem (OneOf44 x) = toElem x
    toElem (TwoOf44 x) = toElem x
    toElem (ThreeOf44 x) = toElem x
    toElem (FourOf44 x) = toElem x
    toElem (FiveOf44 x) = toElem x
    toElem (SixOf44 x) = toElem x
    toElem (SevenOf44 x) = toElem x
    toElem (EightOf44 x) = toElem x
    toElem (NineOf44 x) = toElem x
    toElem (TenOf44 x) = toElem x
    toElem (ElevenOf44 x) = toElem x
    toElem (TwelveOf44 x) = toElem x
    toElem (ThirteenOf44 x) = toElem x
    toElem (FourteenOf44 x) = toElem x
    toElem (FifteenOf44 x) = toElem x
    toElem (SixteenOf44 x) = toElem x
    toElem (SeventeenOf44 x) = toElem x
    toElem (EighteenOf44 x) = toElem x
    toElem (NineteenOf44 x) = toElem x
    toElem (TwentyOf44 x) = toElem x
    toElem (Choice21Of44 x) = toElem x
    toElem (Choice22Of44 x) = toElem x
    toElem (Choice23Of44 x) = toElem x
    toElem (Choice24Of44 x) = toElem x
    toElem (Choice25Of44 x) = toElem x
    toElem (Choice26Of44 x) = toElem x
    toElem (Choice27Of44 x) = toElem x
    toElem (Choice28Of44 x) = toElem x
    toElem (Choice29Of44 x) = toElem x
    toElem (Choice30Of44 x) = toElem x
    toElem (Choice31Of44 x) = toElem x
    toElem (Choice32Of44 x) = toElem x
    toElem (Choice33Of44 x) = toElem x
    toElem (Choice34Of44 x) = toElem x
    toElem (Choice35Of44 x) = toElem x
    toElem (Choice36Of44 x) = toElem x
    toElem (Choice37Of44 x) = toElem x
    toElem (Choice38Of44 x) = toElem x
    toElem (Choice39Of44 x) = toElem x
    toElem (Choice40Of44 x) = toElem x
    toElem (Choice41Of44 x) = toElem x
    toElem (Choice42Of44 x) = toElem x
    toElem (Choice43Of44 x) = toElem x
    toElem (Choice44Of44 x) = toElem x

----
data OneOf45 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as
    = OneOf45 a | TwoOf45 b | ThreeOf45 c | FourOf45 d | FiveOf45 e
    | SixOf45 f | SevenOf45 g | EightOf45 h | NineOf45 i | TenOf45 j
    | ElevenOf45 k | TwelveOf45 l | ThirteenOf45 m | FourteenOf45 n
    | FifteenOf45 o | SixteenOf45 p | SeventeenOf45 q | EighteenOf45 r
    | NineteenOf45 s | TwentyOf45 t | Choice21Of45 u | Choice22Of45 v
    | Choice23Of45 w | Choice24Of45 x | Choice25Of45 y | Choice26Of45 z
    | Choice27Of45 aa | Choice28Of45 ab | Choice29Of45 ac | Choice30Of45 ad
    | Choice31Of45 ae | Choice32Of45 af | Choice33Of45 ag | Choice34Of45 ah
    | Choice35Of45 ai | Choice36Of45 aj | Choice37Of45 ak | Choice38Of45 al
    | Choice39Of45 am | Choice40Of45 an | Choice41Of45 ao | Choice42Of45 ap
    | Choice43Of45 aq | Choice44Of45 ar | Choice45Of45 as
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as)
    => XmlContent (OneOf45 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as)
  where
    fromElem cs =
        (choice OneOf45 $ choice TwoOf45 $ choice ThreeOf45 $ choice FourOf45
        $ choice FiveOf45 $ choice SixOf45 $ choice SevenOf45
        $ choice EightOf45 $ choice NineOf45 $ choice TenOf45
        $ choice ElevenOf45 $ choice TwelveOf45 $ choice ThirteenOf45
        $ choice FourteenOf45 $ choice FifteenOf45 $ choice SixteenOf45
        $ choice SeventeenOf45 $ choice EighteenOf45 $ choice NineteenOf45
        $ choice TwentyOf45 $ choice Choice21Of45 $ choice Choice22Of45
        $ choice Choice23Of45 $ choice Choice24Of45 $ choice Choice25Of45
        $ choice Choice26Of45 $ choice Choice27Of45 $ choice Choice28Of45
        $ choice Choice29Of45 $ choice Choice30Of45 $ choice Choice31Of45
        $ choice Choice32Of45 $ choice Choice33Of45 $ choice Choice34Of45
        $ choice Choice35Of45 $ choice Choice36Of45 $ choice Choice37Of45
        $ choice Choice38Of45 $ choice Choice39Of45 $ choice Choice40Of45
        $ choice Choice41Of45 $ choice Choice42Of45 $ choice Choice43Of45
        $ choice Choice44Of45 $ choice Choice45Of45
        $ (\c->(Nothing,c))) cs
    toElem (OneOf45 x) = toElem x
    toElem (TwoOf45 x) = toElem x
    toElem (ThreeOf45 x) = toElem x
    toElem (FourOf45 x) = toElem x
    toElem (FiveOf45 x) = toElem x
    toElem (SixOf45 x) = toElem x
    toElem (SevenOf45 x) = toElem x
    toElem (EightOf45 x) = toElem x
    toElem (NineOf45 x) = toElem x
    toElem (TenOf45 x) = toElem x
    toElem (ElevenOf45 x) = toElem x
    toElem (TwelveOf45 x) = toElem x
    toElem (ThirteenOf45 x) = toElem x
    toElem (FourteenOf45 x) = toElem x
    toElem (FifteenOf45 x) = toElem x
    toElem (SixteenOf45 x) = toElem x
    toElem (SeventeenOf45 x) = toElem x
    toElem (EighteenOf45 x) = toElem x
    toElem (NineteenOf45 x) = toElem x
    toElem (TwentyOf45 x) = toElem x
    toElem (Choice21Of45 x) = toElem x
    toElem (Choice22Of45 x) = toElem x
    toElem (Choice23Of45 x) = toElem x
    toElem (Choice24Of45 x) = toElem x
    toElem (Choice25Of45 x) = toElem x
    toElem (Choice26Of45 x) = toElem x
    toElem (Choice27Of45 x) = toElem x
    toElem (Choice28Of45 x) = toElem x
    toElem (Choice29Of45 x) = toElem x
    toElem (Choice30Of45 x) = toElem x
    toElem (Choice31Of45 x) = toElem x
    toElem (Choice32Of45 x) = toElem x
    toElem (Choice33Of45 x) = toElem x
    toElem (Choice34Of45 x) = toElem x
    toElem (Choice35Of45 x) = toElem x
    toElem (Choice36Of45 x) = toElem x
    toElem (Choice37Of45 x) = toElem x
    toElem (Choice38Of45 x) = toElem x
    toElem (Choice39Of45 x) = toElem x
    toElem (Choice40Of45 x) = toElem x
    toElem (Choice41Of45 x) = toElem x
    toElem (Choice42Of45 x) = toElem x
    toElem (Choice43Of45 x) = toElem x
    toElem (Choice44Of45 x) = toElem x
    toElem (Choice45Of45 x) = toElem x

----
data OneOf46 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at
    = OneOf46 a | TwoOf46 b | ThreeOf46 c | FourOf46 d | FiveOf46 e
    | SixOf46 f | SevenOf46 g | EightOf46 h | NineOf46 i | TenOf46 j
    | ElevenOf46 k | TwelveOf46 l | ThirteenOf46 m | FourteenOf46 n
    | FifteenOf46 o | SixteenOf46 p | SeventeenOf46 q | EighteenOf46 r
    | NineteenOf46 s | TwentyOf46 t | Choice21Of46 u | Choice22Of46 v
    | Choice23Of46 w | Choice24Of46 x | Choice25Of46 y | Choice26Of46 z
    | Choice27Of46 aa | Choice28Of46 ab | Choice29Of46 ac | Choice30Of46 ad
    | Choice31Of46 ae | Choice32Of46 af | Choice33Of46 ag | Choice34Of46 ah
    | Choice35Of46 ai | Choice36Of46 aj | Choice37Of46 ak | Choice38Of46 al
    | Choice39Of46 am | Choice40Of46 an | Choice41Of46 ao | Choice42Of46 ap
    | Choice43Of46 aq | Choice44Of46 ar | Choice45Of46 as | Choice46Of46 at
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at)
    => XmlContent (OneOf46 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at)
  where
    fromElem cs =
        (choice OneOf46 $ choice TwoOf46 $ choice ThreeOf46 $ choice FourOf46
        $ choice FiveOf46 $ choice SixOf46 $ choice SevenOf46
        $ choice EightOf46 $ choice NineOf46 $ choice TenOf46
        $ choice ElevenOf46 $ choice TwelveOf46 $ choice ThirteenOf46
        $ choice FourteenOf46 $ choice FifteenOf46 $ choice SixteenOf46
        $ choice SeventeenOf46 $ choice EighteenOf46 $ choice NineteenOf46
        $ choice TwentyOf46 $ choice Choice21Of46 $ choice Choice22Of46
        $ choice Choice23Of46 $ choice Choice24Of46 $ choice Choice25Of46
        $ choice Choice26Of46 $ choice Choice27Of46 $ choice Choice28Of46
        $ choice Choice29Of46 $ choice Choice30Of46 $ choice Choice31Of46
        $ choice Choice32Of46 $ choice Choice33Of46 $ choice Choice34Of46
        $ choice Choice35Of46 $ choice Choice36Of46 $ choice Choice37Of46
        $ choice Choice38Of46 $ choice Choice39Of46 $ choice Choice40Of46
        $ choice Choice41Of46 $ choice Choice42Of46 $ choice Choice43Of46
        $ choice Choice44Of46 $ choice Choice45Of46 $ choice Choice46Of46
        $ (\c->(Nothing,c))) cs
    toElem (OneOf46 x) = toElem x
    toElem (TwoOf46 x) = toElem x
    toElem (ThreeOf46 x) = toElem x
    toElem (FourOf46 x) = toElem x
    toElem (FiveOf46 x) = toElem x
    toElem (SixOf46 x) = toElem x
    toElem (SevenOf46 x) = toElem x
    toElem (EightOf46 x) = toElem x
    toElem (NineOf46 x) = toElem x
    toElem (TenOf46 x) = toElem x
    toElem (ElevenOf46 x) = toElem x
    toElem (TwelveOf46 x) = toElem x
    toElem (ThirteenOf46 x) = toElem x
    toElem (FourteenOf46 x) = toElem x
    toElem (FifteenOf46 x) = toElem x
    toElem (SixteenOf46 x) = toElem x
    toElem (SeventeenOf46 x) = toElem x
    toElem (EighteenOf46 x) = toElem x
    toElem (NineteenOf46 x) = toElem x
    toElem (TwentyOf46 x) = toElem x
    toElem (Choice21Of46 x) = toElem x
    toElem (Choice22Of46 x) = toElem x
    toElem (Choice23Of46 x) = toElem x
    toElem (Choice24Of46 x) = toElem x
    toElem (Choice25Of46 x) = toElem x
    toElem (Choice26Of46 x) = toElem x
    toElem (Choice27Of46 x) = toElem x
    toElem (Choice28Of46 x) = toElem x
    toElem (Choice29Of46 x) = toElem x
    toElem (Choice30Of46 x) = toElem x
    toElem (Choice31Of46 x) = toElem x
    toElem (Choice32Of46 x) = toElem x
    toElem (Choice33Of46 x) = toElem x
    toElem (Choice34Of46 x) = toElem x
    toElem (Choice35Of46 x) = toElem x
    toElem (Choice36Of46 x) = toElem x
    toElem (Choice37Of46 x) = toElem x
    toElem (Choice38Of46 x) = toElem x
    toElem (Choice39Of46 x) = toElem x
    toElem (Choice40Of46 x) = toElem x
    toElem (Choice41Of46 x) = toElem x
    toElem (Choice42Of46 x) = toElem x
    toElem (Choice43Of46 x) = toElem x
    toElem (Choice44Of46 x) = toElem x
    toElem (Choice45Of46 x) = toElem x
    toElem (Choice46Of46 x) = toElem x

----
data OneOf47 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au
    = OneOf47 a | TwoOf47 b | ThreeOf47 c | FourOf47 d | FiveOf47 e
    | SixOf47 f | SevenOf47 g | EightOf47 h | NineOf47 i | TenOf47 j
    | ElevenOf47 k | TwelveOf47 l | ThirteenOf47 m | FourteenOf47 n
    | FifteenOf47 o | SixteenOf47 p | SeventeenOf47 q | EighteenOf47 r
    | NineteenOf47 s | TwentyOf47 t | Choice21Of47 u | Choice22Of47 v
    | Choice23Of47 w | Choice24Of47 x | Choice25Of47 y | Choice26Of47 z
    | Choice27Of47 aa | Choice28Of47 ab | Choice29Of47 ac | Choice30Of47 ad
    | Choice31Of47 ae | Choice32Of47 af | Choice33Of47 ag | Choice34Of47 ah
    | Choice35Of47 ai | Choice36Of47 aj | Choice37Of47 ak | Choice38Of47 al
    | Choice39Of47 am | Choice40Of47 an | Choice41Of47 ao | Choice42Of47 ap
    | Choice43Of47 aq | Choice44Of47 ar | Choice45Of47 as | Choice46Of47 at
    | Choice47Of47 au
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au)
    => XmlContent (OneOf47 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au)
  where
    fromElem cs =
        (choice OneOf47 $ choice TwoOf47 $ choice ThreeOf47 $ choice FourOf47
        $ choice FiveOf47 $ choice SixOf47 $ choice SevenOf47
        $ choice EightOf47 $ choice NineOf47 $ choice TenOf47
        $ choice ElevenOf47 $ choice TwelveOf47 $ choice ThirteenOf47
        $ choice FourteenOf47 $ choice FifteenOf47 $ choice SixteenOf47
        $ choice SeventeenOf47 $ choice EighteenOf47 $ choice NineteenOf47
        $ choice TwentyOf47 $ choice Choice21Of47 $ choice Choice22Of47
        $ choice Choice23Of47 $ choice Choice24Of47 $ choice Choice25Of47
        $ choice Choice26Of47 $ choice Choice27Of47 $ choice Choice28Of47
        $ choice Choice29Of47 $ choice Choice30Of47 $ choice Choice31Of47
        $ choice Choice32Of47 $ choice Choice33Of47 $ choice Choice34Of47
        $ choice Choice35Of47 $ choice Choice36Of47 $ choice Choice37Of47
        $ choice Choice38Of47 $ choice Choice39Of47 $ choice Choice40Of47
        $ choice Choice41Of47 $ choice Choice42Of47 $ choice Choice43Of47
        $ choice Choice44Of47 $ choice Choice45Of47 $ choice Choice46Of47
        $ choice Choice47Of47
        $ (\c->(Nothing,c))) cs
    toElem (OneOf47 x) = toElem x
    toElem (TwoOf47 x) = toElem x
    toElem (ThreeOf47 x) = toElem x
    toElem (FourOf47 x) = toElem x
    toElem (FiveOf47 x) = toElem x
    toElem (SixOf47 x) = toElem x
    toElem (SevenOf47 x) = toElem x
    toElem (EightOf47 x) = toElem x
    toElem (NineOf47 x) = toElem x
    toElem (TenOf47 x) = toElem x
    toElem (ElevenOf47 x) = toElem x
    toElem (TwelveOf47 x) = toElem x
    toElem (ThirteenOf47 x) = toElem x
    toElem (FourteenOf47 x) = toElem x
    toElem (FifteenOf47 x) = toElem x
    toElem (SixteenOf47 x) = toElem x
    toElem (SeventeenOf47 x) = toElem x
    toElem (EighteenOf47 x) = toElem x
    toElem (NineteenOf47 x) = toElem x
    toElem (TwentyOf47 x) = toElem x
    toElem (Choice21Of47 x) = toElem x
    toElem (Choice22Of47 x) = toElem x
    toElem (Choice23Of47 x) = toElem x
    toElem (Choice24Of47 x) = toElem x
    toElem (Choice25Of47 x) = toElem x
    toElem (Choice26Of47 x) = toElem x
    toElem (Choice27Of47 x) = toElem x
    toElem (Choice28Of47 x) = toElem x
    toElem (Choice29Of47 x) = toElem x
    toElem (Choice30Of47 x) = toElem x
    toElem (Choice31Of47 x) = toElem x
    toElem (Choice32Of47 x) = toElem x
    toElem (Choice33Of47 x) = toElem x
    toElem (Choice34Of47 x) = toElem x
    toElem (Choice35Of47 x) = toElem x
    toElem (Choice36Of47 x) = toElem x
    toElem (Choice37Of47 x) = toElem x
    toElem (Choice38Of47 x) = toElem x
    toElem (Choice39Of47 x) = toElem x
    toElem (Choice40Of47 x) = toElem x
    toElem (Choice41Of47 x) = toElem x
    toElem (Choice42Of47 x) = toElem x
    toElem (Choice43Of47 x) = toElem x
    toElem (Choice44Of47 x) = toElem x
    toElem (Choice45Of47 x) = toElem x
    toElem (Choice46Of47 x) = toElem x
    toElem (Choice47Of47 x) = toElem x

----
data OneOf48 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av
    = OneOf48 a | TwoOf48 b | ThreeOf48 c | FourOf48 d | FiveOf48 e
    | SixOf48 f | SevenOf48 g | EightOf48 h | NineOf48 i | TenOf48 j
    | ElevenOf48 k | TwelveOf48 l | ThirteenOf48 m | FourteenOf48 n
    | FifteenOf48 o | SixteenOf48 p | SeventeenOf48 q | EighteenOf48 r
    | NineteenOf48 s | TwentyOf48 t | Choice21Of48 u | Choice22Of48 v
    | Choice23Of48 w | Choice24Of48 x | Choice25Of48 y | Choice26Of48 z
    | Choice27Of48 aa | Choice28Of48 ab | Choice29Of48 ac | Choice30Of48 ad
    | Choice31Of48 ae | Choice32Of48 af | Choice33Of48 ag | Choice34Of48 ah
    | Choice35Of48 ai | Choice36Of48 aj | Choice37Of48 ak | Choice38Of48 al
    | Choice39Of48 am | Choice40Of48 an | Choice41Of48 ao | Choice42Of48 ap
    | Choice43Of48 aq | Choice44Of48 ar | Choice45Of48 as | Choice46Of48 at
    | Choice47Of48 au | Choice48Of48 av
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av)
    => XmlContent (OneOf48 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av)
  where
    fromElem cs =
        (choice OneOf48 $ choice TwoOf48 $ choice ThreeOf48 $ choice FourOf48
        $ choice FiveOf48 $ choice SixOf48 $ choice SevenOf48
        $ choice EightOf48 $ choice NineOf48 $ choice TenOf48
        $ choice ElevenOf48 $ choice TwelveOf48 $ choice ThirteenOf48
        $ choice FourteenOf48 $ choice FifteenOf48 $ choice SixteenOf48
        $ choice SeventeenOf48 $ choice EighteenOf48 $ choice NineteenOf48
        $ choice TwentyOf48 $ choice Choice21Of48 $ choice Choice22Of48
        $ choice Choice23Of48 $ choice Choice24Of48 $ choice Choice25Of48
        $ choice Choice26Of48 $ choice Choice27Of48 $ choice Choice28Of48
        $ choice Choice29Of48 $ choice Choice30Of48 $ choice Choice31Of48
        $ choice Choice32Of48 $ choice Choice33Of48 $ choice Choice34Of48
        $ choice Choice35Of48 $ choice Choice36Of48 $ choice Choice37Of48
        $ choice Choice38Of48 $ choice Choice39Of48 $ choice Choice40Of48
        $ choice Choice41Of48 $ choice Choice42Of48 $ choice Choice43Of48
        $ choice Choice44Of48 $ choice Choice45Of48 $ choice Choice46Of48
        $ choice Choice47Of48 $ choice Choice48Of48
        $ (\c->(Nothing,c))) cs
    toElem (OneOf48 x) = toElem x
    toElem (TwoOf48 x) = toElem x
    toElem (ThreeOf48 x) = toElem x
    toElem (FourOf48 x) = toElem x
    toElem (FiveOf48 x) = toElem x
    toElem (SixOf48 x) = toElem x
    toElem (SevenOf48 x) = toElem x
    toElem (EightOf48 x) = toElem x
    toElem (NineOf48 x) = toElem x
    toElem (TenOf48 x) = toElem x
    toElem (ElevenOf48 x) = toElem x
    toElem (TwelveOf48 x) = toElem x
    toElem (ThirteenOf48 x) = toElem x
    toElem (FourteenOf48 x) = toElem x
    toElem (FifteenOf48 x) = toElem x
    toElem (SixteenOf48 x) = toElem x
    toElem (SeventeenOf48 x) = toElem x
    toElem (EighteenOf48 x) = toElem x
    toElem (NineteenOf48 x) = toElem x
    toElem (TwentyOf48 x) = toElem x
    toElem (Choice21Of48 x) = toElem x
    toElem (Choice22Of48 x) = toElem x
    toElem (Choice23Of48 x) = toElem x
    toElem (Choice24Of48 x) = toElem x
    toElem (Choice25Of48 x) = toElem x
    toElem (Choice26Of48 x) = toElem x
    toElem (Choice27Of48 x) = toElem x
    toElem (Choice28Of48 x) = toElem x
    toElem (Choice29Of48 x) = toElem x
    toElem (Choice30Of48 x) = toElem x
    toElem (Choice31Of48 x) = toElem x
    toElem (Choice32Of48 x) = toElem x
    toElem (Choice33Of48 x) = toElem x
    toElem (Choice34Of48 x) = toElem x
    toElem (Choice35Of48 x) = toElem x
    toElem (Choice36Of48 x) = toElem x
    toElem (Choice37Of48 x) = toElem x
    toElem (Choice38Of48 x) = toElem x
    toElem (Choice39Of48 x) = toElem x
    toElem (Choice40Of48 x) = toElem x
    toElem (Choice41Of48 x) = toElem x
    toElem (Choice42Of48 x) = toElem x
    toElem (Choice43Of48 x) = toElem x
    toElem (Choice44Of48 x) = toElem x
    toElem (Choice45Of48 x) = toElem x
    toElem (Choice46Of48 x) = toElem x
    toElem (Choice47Of48 x) = toElem x
    toElem (Choice48Of48 x) = toElem x

----
data OneOf49 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw
    = OneOf49 a | TwoOf49 b | ThreeOf49 c | FourOf49 d | FiveOf49 e
    | SixOf49 f | SevenOf49 g | EightOf49 h | NineOf49 i | TenOf49 j
    | ElevenOf49 k | TwelveOf49 l | ThirteenOf49 m | FourteenOf49 n
    | FifteenOf49 o | SixteenOf49 p | SeventeenOf49 q | EighteenOf49 r
    | NineteenOf49 s | TwentyOf49 t | Choice21Of49 u | Choice22Of49 v
    | Choice23Of49 w | Choice24Of49 x | Choice25Of49 y | Choice26Of49 z
    | Choice27Of49 aa | Choice28Of49 ab | Choice29Of49 ac | Choice30Of49 ad
    | Choice31Of49 ae | Choice32Of49 af | Choice33Of49 ag | Choice34Of49 ah
    | Choice35Of49 ai | Choice36Of49 aj | Choice37Of49 ak | Choice38Of49 al
    | Choice39Of49 am | Choice40Of49 an | Choice41Of49 ao | Choice42Of49 ap
    | Choice43Of49 aq | Choice44Of49 ar | Choice45Of49 as | Choice46Of49 at
    | Choice47Of49 au | Choice48Of49 av | Choice49Of49 aw
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw)
    => XmlContent (OneOf49 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw)
  where
    fromElem cs =
        (choice OneOf49 $ choice TwoOf49 $ choice ThreeOf49 $ choice FourOf49
        $ choice FiveOf49 $ choice SixOf49 $ choice SevenOf49
        $ choice EightOf49 $ choice NineOf49 $ choice TenOf49
        $ choice ElevenOf49 $ choice TwelveOf49 $ choice ThirteenOf49
        $ choice FourteenOf49 $ choice FifteenOf49 $ choice SixteenOf49
        $ choice SeventeenOf49 $ choice EighteenOf49 $ choice NineteenOf49
        $ choice TwentyOf49 $ choice Choice21Of49 $ choice Choice22Of49
        $ choice Choice23Of49 $ choice Choice24Of49 $ choice Choice25Of49
        $ choice Choice26Of49 $ choice Choice27Of49 $ choice Choice28Of49
        $ choice Choice29Of49 $ choice Choice30Of49 $ choice Choice31Of49
        $ choice Choice32Of49 $ choice Choice33Of49 $ choice Choice34Of49
        $ choice Choice35Of49 $ choice Choice36Of49 $ choice Choice37Of49
        $ choice Choice38Of49 $ choice Choice39Of49 $ choice Choice40Of49
        $ choice Choice41Of49 $ choice Choice42Of49 $ choice Choice43Of49
        $ choice Choice44Of49 $ choice Choice45Of49 $ choice Choice46Of49
        $ choice Choice47Of49 $ choice Choice48Of49 $ choice Choice49Of49
        $ (\c->(Nothing,c))) cs
    toElem (OneOf49 x) = toElem x
    toElem (TwoOf49 x) = toElem x
    toElem (ThreeOf49 x) = toElem x
    toElem (FourOf49 x) = toElem x
    toElem (FiveOf49 x) = toElem x
    toElem (SixOf49 x) = toElem x
    toElem (SevenOf49 x) = toElem x
    toElem (EightOf49 x) = toElem x
    toElem (NineOf49 x) = toElem x
    toElem (TenOf49 x) = toElem x
    toElem (ElevenOf49 x) = toElem x
    toElem (TwelveOf49 x) = toElem x
    toElem (ThirteenOf49 x) = toElem x
    toElem (FourteenOf49 x) = toElem x
    toElem (FifteenOf49 x) = toElem x
    toElem (SixteenOf49 x) = toElem x
    toElem (SeventeenOf49 x) = toElem x
    toElem (EighteenOf49 x) = toElem x
    toElem (NineteenOf49 x) = toElem x
    toElem (TwentyOf49 x) = toElem x
    toElem (Choice21Of49 x) = toElem x
    toElem (Choice22Of49 x) = toElem x
    toElem (Choice23Of49 x) = toElem x
    toElem (Choice24Of49 x) = toElem x
    toElem (Choice25Of49 x) = toElem x
    toElem (Choice26Of49 x) = toElem x
    toElem (Choice27Of49 x) = toElem x
    toElem (Choice28Of49 x) = toElem x
    toElem (Choice29Of49 x) = toElem x
    toElem (Choice30Of49 x) = toElem x
    toElem (Choice31Of49 x) = toElem x
    toElem (Choice32Of49 x) = toElem x
    toElem (Choice33Of49 x) = toElem x
    toElem (Choice34Of49 x) = toElem x
    toElem (Choice35Of49 x) = toElem x
    toElem (Choice36Of49 x) = toElem x
    toElem (Choice37Of49 x) = toElem x
    toElem (Choice38Of49 x) = toElem x
    toElem (Choice39Of49 x) = toElem x
    toElem (Choice40Of49 x) = toElem x
    toElem (Choice41Of49 x) = toElem x
    toElem (Choice42Of49 x) = toElem x
    toElem (Choice43Of49 x) = toElem x
    toElem (Choice44Of49 x) = toElem x
    toElem (Choice45Of49 x) = toElem x
    toElem (Choice46Of49 x) = toElem x
    toElem (Choice47Of49 x) = toElem x
    toElem (Choice48Of49 x) = toElem x
    toElem (Choice49Of49 x) = toElem x

----
data OneOf50 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax
    = OneOf50 a | TwoOf50 b | ThreeOf50 c | FourOf50 d | FiveOf50 e
    | SixOf50 f | SevenOf50 g | EightOf50 h | NineOf50 i | TenOf50 j
    | ElevenOf50 k | TwelveOf50 l | ThirteenOf50 m | FourteenOf50 n
    | FifteenOf50 o | SixteenOf50 p | SeventeenOf50 q | EighteenOf50 r
    | NineteenOf50 s | TwentyOf50 t | Choice21Of50 u | Choice22Of50 v
    | Choice23Of50 w | Choice24Of50 x | Choice25Of50 y | Choice26Of50 z
    | Choice27Of50 aa | Choice28Of50 ab | Choice29Of50 ac | Choice30Of50 ad
    | Choice31Of50 ae | Choice32Of50 af | Choice33Of50 ag | Choice34Of50 ah
    | Choice35Of50 ai | Choice36Of50 aj | Choice37Of50 ak | Choice38Of50 al
    | Choice39Of50 am | Choice40Of50 an | Choice41Of50 ao | Choice42Of50 ap
    | Choice43Of50 aq | Choice44Of50 ar | Choice45Of50 as | Choice46Of50 at
    | Choice47Of50 au | Choice48Of50 av | Choice49Of50 aw | Choice50Of50 ax
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax)
    => XmlContent (OneOf50 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax)
  where
    fromElem cs =
        (choice OneOf50 $ choice TwoOf50 $ choice ThreeOf50 $ choice FourOf50
        $ choice FiveOf50 $ choice SixOf50 $ choice SevenOf50
        $ choice EightOf50 $ choice NineOf50 $ choice TenOf50
        $ choice ElevenOf50 $ choice TwelveOf50 $ choice ThirteenOf50
        $ choice FourteenOf50 $ choice FifteenOf50 $ choice SixteenOf50
        $ choice SeventeenOf50 $ choice EighteenOf50 $ choice NineteenOf50
        $ choice TwentyOf50 $ choice Choice21Of50 $ choice Choice22Of50
        $ choice Choice23Of50 $ choice Choice24Of50 $ choice Choice25Of50
        $ choice Choice26Of50 $ choice Choice27Of50 $ choice Choice28Of50
        $ choice Choice29Of50 $ choice Choice30Of50 $ choice Choice31Of50
        $ choice Choice32Of50 $ choice Choice33Of50 $ choice Choice34Of50
        $ choice Choice35Of50 $ choice Choice36Of50 $ choice Choice37Of50
        $ choice Choice38Of50 $ choice Choice39Of50 $ choice Choice40Of50
        $ choice Choice41Of50 $ choice Choice42Of50 $ choice Choice43Of50
        $ choice Choice44Of50 $ choice Choice45Of50 $ choice Choice46Of50
        $ choice Choice47Of50 $ choice Choice48Of50 $ choice Choice49Of50
        $ choice Choice50Of50
        $ (\c->(Nothing,c))) cs
    toElem (OneOf50 x) = toElem x
    toElem (TwoOf50 x) = toElem x
    toElem (ThreeOf50 x) = toElem x
    toElem (FourOf50 x) = toElem x
    toElem (FiveOf50 x) = toElem x
    toElem (SixOf50 x) = toElem x
    toElem (SevenOf50 x) = toElem x
    toElem (EightOf50 x) = toElem x
    toElem (NineOf50 x) = toElem x
    toElem (TenOf50 x) = toElem x
    toElem (ElevenOf50 x) = toElem x
    toElem (TwelveOf50 x) = toElem x
    toElem (ThirteenOf50 x) = toElem x
    toElem (FourteenOf50 x) = toElem x
    toElem (FifteenOf50 x) = toElem x
    toElem (SixteenOf50 x) = toElem x
    toElem (SeventeenOf50 x) = toElem x
    toElem (EighteenOf50 x) = toElem x
    toElem (NineteenOf50 x) = toElem x
    toElem (TwentyOf50 x) = toElem x
    toElem (Choice21Of50 x) = toElem x
    toElem (Choice22Of50 x) = toElem x
    toElem (Choice23Of50 x) = toElem x
    toElem (Choice24Of50 x) = toElem x
    toElem (Choice25Of50 x) = toElem x
    toElem (Choice26Of50 x) = toElem x
    toElem (Choice27Of50 x) = toElem x
    toElem (Choice28Of50 x) = toElem x
    toElem (Choice29Of50 x) = toElem x
    toElem (Choice30Of50 x) = toElem x
    toElem (Choice31Of50 x) = toElem x
    toElem (Choice32Of50 x) = toElem x
    toElem (Choice33Of50 x) = toElem x
    toElem (Choice34Of50 x) = toElem x
    toElem (Choice35Of50 x) = toElem x
    toElem (Choice36Of50 x) = toElem x
    toElem (Choice37Of50 x) = toElem x
    toElem (Choice38Of50 x) = toElem x
    toElem (Choice39Of50 x) = toElem x
    toElem (Choice40Of50 x) = toElem x
    toElem (Choice41Of50 x) = toElem x
    toElem (Choice42Of50 x) = toElem x
    toElem (Choice43Of50 x) = toElem x
    toElem (Choice44Of50 x) = toElem x
    toElem (Choice45Of50 x) = toElem x
    toElem (Choice46Of50 x) = toElem x
    toElem (Choice47Of50 x) = toElem x
    toElem (Choice48Of50 x) = toElem x
    toElem (Choice49Of50 x) = toElem x
    toElem (Choice50Of50 x) = toElem x

----
data OneOf51 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
    = OneOf51 a | TwoOf51 b | ThreeOf51 c | FourOf51 d | FiveOf51 e
    | SixOf51 f | SevenOf51 g | EightOf51 h | NineOf51 i | TenOf51 j
    | ElevenOf51 k | TwelveOf51 l | ThirteenOf51 m | FourteenOf51 n
    | FifteenOf51 o | SixteenOf51 p | SeventeenOf51 q | EighteenOf51 r
    | NineteenOf51 s | TwentyOf51 t | Choice21Of51 u | Choice22Of51 v
    | Choice23Of51 w | Choice24Of51 x | Choice25Of51 y | Choice26Of51 z
    | Choice27Of51 aa | Choice28Of51 ab | Choice29Of51 ac | Choice30Of51 ad
    | Choice31Of51 ae | Choice32Of51 af | Choice33Of51 ag | Choice34Of51 ah
    | Choice35Of51 ai | Choice36Of51 aj | Choice37Of51 ak | Choice38Of51 al
    | Choice39Of51 am | Choice40Of51 an | Choice41Of51 ao | Choice42Of51 ap
    | Choice43Of51 aq | Choice44Of51 ar | Choice45Of51 as | Choice46Of51 at
    | Choice47Of51 au | Choice48Of51 av | Choice49Of51 aw | Choice50Of51 ax
    | Choice51Of51 ay
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay)
    => XmlContent (OneOf51 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay)
  where
    fromElem cs =
        (choice OneOf51 $ choice TwoOf51 $ choice ThreeOf51 $ choice FourOf51
        $ choice FiveOf51 $ choice SixOf51 $ choice SevenOf51
        $ choice EightOf51 $ choice NineOf51 $ choice TenOf51
        $ choice ElevenOf51 $ choice TwelveOf51 $ choice ThirteenOf51
        $ choice FourteenOf51 $ choice FifteenOf51 $ choice SixteenOf51
        $ choice SeventeenOf51 $ choice EighteenOf51 $ choice NineteenOf51
        $ choice TwentyOf51 $ choice Choice21Of51 $ choice Choice22Of51
        $ choice Choice23Of51 $ choice Choice24Of51 $ choice Choice25Of51
        $ choice Choice26Of51 $ choice Choice27Of51 $ choice Choice28Of51
        $ choice Choice29Of51 $ choice Choice30Of51 $ choice Choice31Of51
        $ choice Choice32Of51 $ choice Choice33Of51 $ choice Choice34Of51
        $ choice Choice35Of51 $ choice Choice36Of51 $ choice Choice37Of51
        $ choice Choice38Of51 $ choice Choice39Of51 $ choice Choice40Of51
        $ choice Choice41Of51 $ choice Choice42Of51 $ choice Choice43Of51
        $ choice Choice44Of51 $ choice Choice45Of51 $ choice Choice46Of51
        $ choice Choice47Of51 $ choice Choice48Of51 $ choice Choice49Of51
        $ choice Choice50Of51 $ choice Choice51Of51
        $ (\c->(Nothing,c))) cs
    toElem (OneOf51 x) = toElem x
    toElem (TwoOf51 x) = toElem x
    toElem (ThreeOf51 x) = toElem x
    toElem (FourOf51 x) = toElem x
    toElem (FiveOf51 x) = toElem x
    toElem (SixOf51 x) = toElem x
    toElem (SevenOf51 x) = toElem x
    toElem (EightOf51 x) = toElem x
    toElem (NineOf51 x) = toElem x
    toElem (TenOf51 x) = toElem x
    toElem (ElevenOf51 x) = toElem x
    toElem (TwelveOf51 x) = toElem x
    toElem (ThirteenOf51 x) = toElem x
    toElem (FourteenOf51 x) = toElem x
    toElem (FifteenOf51 x) = toElem x
    toElem (SixteenOf51 x) = toElem x
    toElem (SeventeenOf51 x) = toElem x
    toElem (EighteenOf51 x) = toElem x
    toElem (NineteenOf51 x) = toElem x
    toElem (TwentyOf51 x) = toElem x
    toElem (Choice21Of51 x) = toElem x
    toElem (Choice22Of51 x) = toElem x
    toElem (Choice23Of51 x) = toElem x
    toElem (Choice24Of51 x) = toElem x
    toElem (Choice25Of51 x) = toElem x
    toElem (Choice26Of51 x) = toElem x
    toElem (Choice27Of51 x) = toElem x
    toElem (Choice28Of51 x) = toElem x
    toElem (Choice29Of51 x) = toElem x
    toElem (Choice30Of51 x) = toElem x
    toElem (Choice31Of51 x) = toElem x
    toElem (Choice32Of51 x) = toElem x
    toElem (Choice33Of51 x) = toElem x
    toElem (Choice34Of51 x) = toElem x
    toElem (Choice35Of51 x) = toElem x
    toElem (Choice36Of51 x) = toElem x
    toElem (Choice37Of51 x) = toElem x
    toElem (Choice38Of51 x) = toElem x
    toElem (Choice39Of51 x) = toElem x
    toElem (Choice40Of51 x) = toElem x
    toElem (Choice41Of51 x) = toElem x
    toElem (Choice42Of51 x) = toElem x
    toElem (Choice43Of51 x) = toElem x
    toElem (Choice44Of51 x) = toElem x
    toElem (Choice45Of51 x) = toElem x
    toElem (Choice46Of51 x) = toElem x
    toElem (Choice47Of51 x) = toElem x
    toElem (Choice48Of51 x) = toElem x
    toElem (Choice49Of51 x) = toElem x
    toElem (Choice50Of51 x) = toElem x
    toElem (Choice51Of51 x) = toElem x

----
data OneOf52 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az
    = OneOf52 a | TwoOf52 b | ThreeOf52 c | FourOf52 d | FiveOf52 e
    | SixOf52 f | SevenOf52 g | EightOf52 h | NineOf52 i | TenOf52 j
    | ElevenOf52 k | TwelveOf52 l | ThirteenOf52 m | FourteenOf52 n
    | FifteenOf52 o | SixteenOf52 p | SeventeenOf52 q | EighteenOf52 r
    | NineteenOf52 s | TwentyOf52 t | Choice21Of52 u | Choice22Of52 v
    | Choice23Of52 w | Choice24Of52 x | Choice25Of52 y | Choice26Of52 z
    | Choice27Of52 aa | Choice28Of52 ab | Choice29Of52 ac | Choice30Of52 ad
    | Choice31Of52 ae | Choice32Of52 af | Choice33Of52 ag | Choice34Of52 ah
    | Choice35Of52 ai | Choice36Of52 aj | Choice37Of52 ak | Choice38Of52 al
    | Choice39Of52 am | Choice40Of52 an | Choice41Of52 ao | Choice42Of52 ap
    | Choice43Of52 aq | Choice44Of52 ar | Choice45Of52 as | Choice46Of52 at
    | Choice47Of52 au | Choice48Of52 av | Choice49Of52 aw | Choice50Of52 ax
    | Choice51Of52 ay | Choice52Of52 az
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az)
    => XmlContent (OneOf52 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az)
  where
    fromElem cs =
        (choice OneOf52 $ choice TwoOf52 $ choice ThreeOf52 $ choice FourOf52
        $ choice FiveOf52 $ choice SixOf52 $ choice SevenOf52
        $ choice EightOf52 $ choice NineOf52 $ choice TenOf52
        $ choice ElevenOf52 $ choice TwelveOf52 $ choice ThirteenOf52
        $ choice FourteenOf52 $ choice FifteenOf52 $ choice SixteenOf52
        $ choice SeventeenOf52 $ choice EighteenOf52 $ choice NineteenOf52
        $ choice TwentyOf52 $ choice Choice21Of52 $ choice Choice22Of52
        $ choice Choice23Of52 $ choice Choice24Of52 $ choice Choice25Of52
        $ choice Choice26Of52 $ choice Choice27Of52 $ choice Choice28Of52
        $ choice Choice29Of52 $ choice Choice30Of52 $ choice Choice31Of52
        $ choice Choice32Of52 $ choice Choice33Of52 $ choice Choice34Of52
        $ choice Choice35Of52 $ choice Choice36Of52 $ choice Choice37Of52
        $ choice Choice38Of52 $ choice Choice39Of52 $ choice Choice40Of52
        $ choice Choice41Of52 $ choice Choice42Of52 $ choice Choice43Of52
        $ choice Choice44Of52 $ choice Choice45Of52 $ choice Choice46Of52
        $ choice Choice47Of52 $ choice Choice48Of52 $ choice Choice49Of52
        $ choice Choice50Of52 $ choice Choice51Of52 $ choice Choice52Of52
        $ (\c->(Nothing,c))) cs
    toElem (OneOf52 x) = toElem x
    toElem (TwoOf52 x) = toElem x
    toElem (ThreeOf52 x) = toElem x
    toElem (FourOf52 x) = toElem x
    toElem (FiveOf52 x) = toElem x
    toElem (SixOf52 x) = toElem x
    toElem (SevenOf52 x) = toElem x
    toElem (EightOf52 x) = toElem x
    toElem (NineOf52 x) = toElem x
    toElem (TenOf52 x) = toElem x
    toElem (ElevenOf52 x) = toElem x
    toElem (TwelveOf52 x) = toElem x
    toElem (ThirteenOf52 x) = toElem x
    toElem (FourteenOf52 x) = toElem x
    toElem (FifteenOf52 x) = toElem x
    toElem (SixteenOf52 x) = toElem x
    toElem (SeventeenOf52 x) = toElem x
    toElem (EighteenOf52 x) = toElem x
    toElem (NineteenOf52 x) = toElem x
    toElem (TwentyOf52 x) = toElem x
    toElem (Choice21Of52 x) = toElem x
    toElem (Choice22Of52 x) = toElem x
    toElem (Choice23Of52 x) = toElem x
    toElem (Choice24Of52 x) = toElem x
    toElem (Choice25Of52 x) = toElem x
    toElem (Choice26Of52 x) = toElem x
    toElem (Choice27Of52 x) = toElem x
    toElem (Choice28Of52 x) = toElem x
    toElem (Choice29Of52 x) = toElem x
    toElem (Choice30Of52 x) = toElem x
    toElem (Choice31Of52 x) = toElem x
    toElem (Choice32Of52 x) = toElem x
    toElem (Choice33Of52 x) = toElem x
    toElem (Choice34Of52 x) = toElem x
    toElem (Choice35Of52 x) = toElem x
    toElem (Choice36Of52 x) = toElem x
    toElem (Choice37Of52 x) = toElem x
    toElem (Choice38Of52 x) = toElem x
    toElem (Choice39Of52 x) = toElem x
    toElem (Choice40Of52 x) = toElem x
    toElem (Choice41Of52 x) = toElem x
    toElem (Choice42Of52 x) = toElem x
    toElem (Choice43Of52 x) = toElem x
    toElem (Choice44Of52 x) = toElem x
    toElem (Choice45Of52 x) = toElem x
    toElem (Choice46Of52 x) = toElem x
    toElem (Choice47Of52 x) = toElem x
    toElem (Choice48Of52 x) = toElem x
    toElem (Choice49Of52 x) = toElem x
    toElem (Choice50Of52 x) = toElem x
    toElem (Choice51Of52 x) = toElem x
    toElem (Choice52Of52 x) = toElem x

----
data OneOf53 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba
    = OneOf53 a | TwoOf53 b | ThreeOf53 c | FourOf53 d | FiveOf53 e
    | SixOf53 f | SevenOf53 g | EightOf53 h | NineOf53 i | TenOf53 j
    | ElevenOf53 k | TwelveOf53 l | ThirteenOf53 m | FourteenOf53 n
    | FifteenOf53 o | SixteenOf53 p | SeventeenOf53 q | EighteenOf53 r
    | NineteenOf53 s | TwentyOf53 t | Choice21Of53 u | Choice22Of53 v
    | Choice23Of53 w | Choice24Of53 x | Choice25Of53 y | Choice26Of53 z
    | Choice27Of53 aa | Choice28Of53 ab | Choice29Of53 ac | Choice30Of53 ad
    | Choice31Of53 ae | Choice32Of53 af | Choice33Of53 ag | Choice34Of53 ah
    | Choice35Of53 ai | Choice36Of53 aj | Choice37Of53 ak | Choice38Of53 al
    | Choice39Of53 am | Choice40Of53 an | Choice41Of53 ao | Choice42Of53 ap
    | Choice43Of53 aq | Choice44Of53 ar | Choice45Of53 as | Choice46Of53 at
    | Choice47Of53 au | Choice48Of53 av | Choice49Of53 aw | Choice50Of53 ax
    | Choice51Of53 ay | Choice52Of53 az | Choice53Of53 ba
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba)
    => XmlContent (OneOf53 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba)
  where
    fromElem cs =
        (choice OneOf53 $ choice TwoOf53 $ choice ThreeOf53 $ choice FourOf53
        $ choice FiveOf53 $ choice SixOf53 $ choice SevenOf53
        $ choice EightOf53 $ choice NineOf53 $ choice TenOf53
        $ choice ElevenOf53 $ choice TwelveOf53 $ choice ThirteenOf53
        $ choice FourteenOf53 $ choice FifteenOf53 $ choice SixteenOf53
        $ choice SeventeenOf53 $ choice EighteenOf53 $ choice NineteenOf53
        $ choice TwentyOf53 $ choice Choice21Of53 $ choice Choice22Of53
        $ choice Choice23Of53 $ choice Choice24Of53 $ choice Choice25Of53
        $ choice Choice26Of53 $ choice Choice27Of53 $ choice Choice28Of53
        $ choice Choice29Of53 $ choice Choice30Of53 $ choice Choice31Of53
        $ choice Choice32Of53 $ choice Choice33Of53 $ choice Choice34Of53
        $ choice Choice35Of53 $ choice Choice36Of53 $ choice Choice37Of53
        $ choice Choice38Of53 $ choice Choice39Of53 $ choice Choice40Of53
        $ choice Choice41Of53 $ choice Choice42Of53 $ choice Choice43Of53
        $ choice Choice44Of53 $ choice Choice45Of53 $ choice Choice46Of53
        $ choice Choice47Of53 $ choice Choice48Of53 $ choice Choice49Of53
        $ choice Choice50Of53 $ choice Choice51Of53 $ choice Choice52Of53
        $ choice Choice53Of53
        $ (\c->(Nothing,c))) cs
    toElem (OneOf53 x) = toElem x
    toElem (TwoOf53 x) = toElem x
    toElem (ThreeOf53 x) = toElem x
    toElem (FourOf53 x) = toElem x
    toElem (FiveOf53 x) = toElem x
    toElem (SixOf53 x) = toElem x
    toElem (SevenOf53 x) = toElem x
    toElem (EightOf53 x) = toElem x
    toElem (NineOf53 x) = toElem x
    toElem (TenOf53 x) = toElem x
    toElem (ElevenOf53 x) = toElem x
    toElem (TwelveOf53 x) = toElem x
    toElem (ThirteenOf53 x) = toElem x
    toElem (FourteenOf53 x) = toElem x
    toElem (FifteenOf53 x) = toElem x
    toElem (SixteenOf53 x) = toElem x
    toElem (SeventeenOf53 x) = toElem x
    toElem (EighteenOf53 x) = toElem x
    toElem (NineteenOf53 x) = toElem x
    toElem (TwentyOf53 x) = toElem x
    toElem (Choice21Of53 x) = toElem x
    toElem (Choice22Of53 x) = toElem x
    toElem (Choice23Of53 x) = toElem x
    toElem (Choice24Of53 x) = toElem x
    toElem (Choice25Of53 x) = toElem x
    toElem (Choice26Of53 x) = toElem x
    toElem (Choice27Of53 x) = toElem x
    toElem (Choice28Of53 x) = toElem x
    toElem (Choice29Of53 x) = toElem x
    toElem (Choice30Of53 x) = toElem x
    toElem (Choice31Of53 x) = toElem x
    toElem (Choice32Of53 x) = toElem x
    toElem (Choice33Of53 x) = toElem x
    toElem (Choice34Of53 x) = toElem x
    toElem (Choice35Of53 x) = toElem x
    toElem (Choice36Of53 x) = toElem x
    toElem (Choice37Of53 x) = toElem x
    toElem (Choice38Of53 x) = toElem x
    toElem (Choice39Of53 x) = toElem x
    toElem (Choice40Of53 x) = toElem x
    toElem (Choice41Of53 x) = toElem x
    toElem (Choice42Of53 x) = toElem x
    toElem (Choice43Of53 x) = toElem x
    toElem (Choice44Of53 x) = toElem x
    toElem (Choice45Of53 x) = toElem x
    toElem (Choice46Of53 x) = toElem x
    toElem (Choice47Of53 x) = toElem x
    toElem (Choice48Of53 x) = toElem x
    toElem (Choice49Of53 x) = toElem x
    toElem (Choice50Of53 x) = toElem x
    toElem (Choice51Of53 x) = toElem x
    toElem (Choice52Of53 x) = toElem x
    toElem (Choice53Of53 x) = toElem x

----
data OneOf54 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb
    = OneOf54 a | TwoOf54 b | ThreeOf54 c | FourOf54 d | FiveOf54 e
    | SixOf54 f | SevenOf54 g | EightOf54 h | NineOf54 i | TenOf54 j
    | ElevenOf54 k | TwelveOf54 l | ThirteenOf54 m | FourteenOf54 n
    | FifteenOf54 o | SixteenOf54 p | SeventeenOf54 q | EighteenOf54 r
    | NineteenOf54 s | TwentyOf54 t | Choice21Of54 u | Choice22Of54 v
    | Choice23Of54 w | Choice24Of54 x | Choice25Of54 y | Choice26Of54 z
    | Choice27Of54 aa | Choice28Of54 ab | Choice29Of54 ac | Choice30Of54 ad
    | Choice31Of54 ae | Choice32Of54 af | Choice33Of54 ag | Choice34Of54 ah
    | Choice35Of54 ai | Choice36Of54 aj | Choice37Of54 ak | Choice38Of54 al
    | Choice39Of54 am | Choice40Of54 an | Choice41Of54 ao | Choice42Of54 ap
    | Choice43Of54 aq | Choice44Of54 ar | Choice45Of54 as | Choice46Of54 at
    | Choice47Of54 au | Choice48Of54 av | Choice49Of54 aw | Choice50Of54 ax
    | Choice51Of54 ay | Choice52Of54 az | Choice53Of54 ba | Choice54Of54 bb
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb)
    => XmlContent (OneOf54 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb)
  where
    fromElem cs =
        (choice OneOf54 $ choice TwoOf54 $ choice ThreeOf54 $ choice FourOf54
        $ choice FiveOf54 $ choice SixOf54 $ choice SevenOf54
        $ choice EightOf54 $ choice NineOf54 $ choice TenOf54
        $ choice ElevenOf54 $ choice TwelveOf54 $ choice ThirteenOf54
        $ choice FourteenOf54 $ choice FifteenOf54 $ choice SixteenOf54
        $ choice SeventeenOf54 $ choice EighteenOf54 $ choice NineteenOf54
        $ choice TwentyOf54 $ choice Choice21Of54 $ choice Choice22Of54
        $ choice Choice23Of54 $ choice Choice24Of54 $ choice Choice25Of54
        $ choice Choice26Of54 $ choice Choice27Of54 $ choice Choice28Of54
        $ choice Choice29Of54 $ choice Choice30Of54 $ choice Choice31Of54
        $ choice Choice32Of54 $ choice Choice33Of54 $ choice Choice34Of54
        $ choice Choice35Of54 $ choice Choice36Of54 $ choice Choice37Of54
        $ choice Choice38Of54 $ choice Choice39Of54 $ choice Choice40Of54
        $ choice Choice41Of54 $ choice Choice42Of54 $ choice Choice43Of54
        $ choice Choice44Of54 $ choice Choice45Of54 $ choice Choice46Of54
        $ choice Choice47Of54 $ choice Choice48Of54 $ choice Choice49Of54
        $ choice Choice50Of54 $ choice Choice51Of54 $ choice Choice52Of54
        $ choice Choice53Of54 $ choice Choice54Of54
        $ (\c->(Nothing,c))) cs
    toElem (OneOf54 x) = toElem x
    toElem (TwoOf54 x) = toElem x
    toElem (ThreeOf54 x) = toElem x
    toElem (FourOf54 x) = toElem x
    toElem (FiveOf54 x) = toElem x
    toElem (SixOf54 x) = toElem x
    toElem (SevenOf54 x) = toElem x
    toElem (EightOf54 x) = toElem x
    toElem (NineOf54 x) = toElem x
    toElem (TenOf54 x) = toElem x
    toElem (ElevenOf54 x) = toElem x
    toElem (TwelveOf54 x) = toElem x
    toElem (ThirteenOf54 x) = toElem x
    toElem (FourteenOf54 x) = toElem x
    toElem (FifteenOf54 x) = toElem x
    toElem (SixteenOf54 x) = toElem x
    toElem (SeventeenOf54 x) = toElem x
    toElem (EighteenOf54 x) = toElem x
    toElem (NineteenOf54 x) = toElem x
    toElem (TwentyOf54 x) = toElem x
    toElem (Choice21Of54 x) = toElem x
    toElem (Choice22Of54 x) = toElem x
    toElem (Choice23Of54 x) = toElem x
    toElem (Choice24Of54 x) = toElem x
    toElem (Choice25Of54 x) = toElem x
    toElem (Choice26Of54 x) = toElem x
    toElem (Choice27Of54 x) = toElem x
    toElem (Choice28Of54 x) = toElem x
    toElem (Choice29Of54 x) = toElem x
    toElem (Choice30Of54 x) = toElem x
    toElem (Choice31Of54 x) = toElem x
    toElem (Choice32Of54 x) = toElem x
    toElem (Choice33Of54 x) = toElem x
    toElem (Choice34Of54 x) = toElem x
    toElem (Choice35Of54 x) = toElem x
    toElem (Choice36Of54 x) = toElem x
    toElem (Choice37Of54 x) = toElem x
    toElem (Choice38Of54 x) = toElem x
    toElem (Choice39Of54 x) = toElem x
    toElem (Choice40Of54 x) = toElem x
    toElem (Choice41Of54 x) = toElem x
    toElem (Choice42Of54 x) = toElem x
    toElem (Choice43Of54 x) = toElem x
    toElem (Choice44Of54 x) = toElem x
    toElem (Choice45Of54 x) = toElem x
    toElem (Choice46Of54 x) = toElem x
    toElem (Choice47Of54 x) = toElem x
    toElem (Choice48Of54 x) = toElem x
    toElem (Choice49Of54 x) = toElem x
    toElem (Choice50Of54 x) = toElem x
    toElem (Choice51Of54 x) = toElem x
    toElem (Choice52Of54 x) = toElem x
    toElem (Choice53Of54 x) = toElem x
    toElem (Choice54Of54 x) = toElem x

----
data OneOf55 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc
    = OneOf55 a | TwoOf55 b | ThreeOf55 c | FourOf55 d | FiveOf55 e
    | SixOf55 f | SevenOf55 g | EightOf55 h | NineOf55 i | TenOf55 j
    | ElevenOf55 k | TwelveOf55 l | ThirteenOf55 m | FourteenOf55 n
    | FifteenOf55 o | SixteenOf55 p | SeventeenOf55 q | EighteenOf55 r
    | NineteenOf55 s | TwentyOf55 t | Choice21Of55 u | Choice22Of55 v
    | Choice23Of55 w | Choice24Of55 x | Choice25Of55 y | Choice26Of55 z
    | Choice27Of55 aa | Choice28Of55 ab | Choice29Of55 ac | Choice30Of55 ad
    | Choice31Of55 ae | Choice32Of55 af | Choice33Of55 ag | Choice34Of55 ah
    | Choice35Of55 ai | Choice36Of55 aj | Choice37Of55 ak | Choice38Of55 al
    | Choice39Of55 am | Choice40Of55 an | Choice41Of55 ao | Choice42Of55 ap
    | Choice43Of55 aq | Choice44Of55 ar | Choice45Of55 as | Choice46Of55 at
    | Choice47Of55 au | Choice48Of55 av | Choice49Of55 aw | Choice50Of55 ax
    | Choice51Of55 ay | Choice52Of55 az | Choice53Of55 ba | Choice54Of55 bb
    | Choice55Of55 bc
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc)
    => XmlContent (OneOf55 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc)
  where
    fromElem cs =
        (choice OneOf55 $ choice TwoOf55 $ choice ThreeOf55 $ choice FourOf55
        $ choice FiveOf55 $ choice SixOf55 $ choice SevenOf55
        $ choice EightOf55 $ choice NineOf55 $ choice TenOf55
        $ choice ElevenOf55 $ choice TwelveOf55 $ choice ThirteenOf55
        $ choice FourteenOf55 $ choice FifteenOf55 $ choice SixteenOf55
        $ choice SeventeenOf55 $ choice EighteenOf55 $ choice NineteenOf55
        $ choice TwentyOf55 $ choice Choice21Of55 $ choice Choice22Of55
        $ choice Choice23Of55 $ choice Choice24Of55 $ choice Choice25Of55
        $ choice Choice26Of55 $ choice Choice27Of55 $ choice Choice28Of55
        $ choice Choice29Of55 $ choice Choice30Of55 $ choice Choice31Of55
        $ choice Choice32Of55 $ choice Choice33Of55 $ choice Choice34Of55
        $ choice Choice35Of55 $ choice Choice36Of55 $ choice Choice37Of55
        $ choice Choice38Of55 $ choice Choice39Of55 $ choice Choice40Of55
        $ choice Choice41Of55 $ choice Choice42Of55 $ choice Choice43Of55
        $ choice Choice44Of55 $ choice Choice45Of55 $ choice Choice46Of55
        $ choice Choice47Of55 $ choice Choice48Of55 $ choice Choice49Of55
        $ choice Choice50Of55 $ choice Choice51Of55 $ choice Choice52Of55
        $ choice Choice53Of55 $ choice Choice54Of55 $ choice Choice55Of55
        $ (\c->(Nothing,c))) cs
    toElem (OneOf55 x) = toElem x
    toElem (TwoOf55 x) = toElem x
    toElem (ThreeOf55 x) = toElem x
    toElem (FourOf55 x) = toElem x
    toElem (FiveOf55 x) = toElem x
    toElem (SixOf55 x) = toElem x
    toElem (SevenOf55 x) = toElem x
    toElem (EightOf55 x) = toElem x
    toElem (NineOf55 x) = toElem x
    toElem (TenOf55 x) = toElem x
    toElem (ElevenOf55 x) = toElem x
    toElem (TwelveOf55 x) = toElem x
    toElem (ThirteenOf55 x) = toElem x
    toElem (FourteenOf55 x) = toElem x
    toElem (FifteenOf55 x) = toElem x
    toElem (SixteenOf55 x) = toElem x
    toElem (SeventeenOf55 x) = toElem x
    toElem (EighteenOf55 x) = toElem x
    toElem (NineteenOf55 x) = toElem x
    toElem (TwentyOf55 x) = toElem x
    toElem (Choice21Of55 x) = toElem x
    toElem (Choice22Of55 x) = toElem x
    toElem (Choice23Of55 x) = toElem x
    toElem (Choice24Of55 x) = toElem x
    toElem (Choice25Of55 x) = toElem x
    toElem (Choice26Of55 x) = toElem x
    toElem (Choice27Of55 x) = toElem x
    toElem (Choice28Of55 x) = toElem x
    toElem (Choice29Of55 x) = toElem x
    toElem (Choice30Of55 x) = toElem x
    toElem (Choice31Of55 x) = toElem x
    toElem (Choice32Of55 x) = toElem x
    toElem (Choice33Of55 x) = toElem x
    toElem (Choice34Of55 x) = toElem x
    toElem (Choice35Of55 x) = toElem x
    toElem (Choice36Of55 x) = toElem x
    toElem (Choice37Of55 x) = toElem x
    toElem (Choice38Of55 x) = toElem x
    toElem (Choice39Of55 x) = toElem x
    toElem (Choice40Of55 x) = toElem x
    toElem (Choice41Of55 x) = toElem x
    toElem (Choice42Of55 x) = toElem x
    toElem (Choice43Of55 x) = toElem x
    toElem (Choice44Of55 x) = toElem x
    toElem (Choice45Of55 x) = toElem x
    toElem (Choice46Of55 x) = toElem x
    toElem (Choice47Of55 x) = toElem x
    toElem (Choice48Of55 x) = toElem x
    toElem (Choice49Of55 x) = toElem x
    toElem (Choice50Of55 x) = toElem x
    toElem (Choice51Of55 x) = toElem x
    toElem (Choice52Of55 x) = toElem x
    toElem (Choice53Of55 x) = toElem x
    toElem (Choice54Of55 x) = toElem x
    toElem (Choice55Of55 x) = toElem x

----
data OneOf56 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd
    = OneOf56 a | TwoOf56 b | ThreeOf56 c | FourOf56 d | FiveOf56 e
    | SixOf56 f | SevenOf56 g | EightOf56 h | NineOf56 i | TenOf56 j
    | ElevenOf56 k | TwelveOf56 l | ThirteenOf56 m | FourteenOf56 n
    | FifteenOf56 o | SixteenOf56 p | SeventeenOf56 q | EighteenOf56 r
    | NineteenOf56 s | TwentyOf56 t | Choice21Of56 u | Choice22Of56 v
    | Choice23Of56 w | Choice24Of56 x | Choice25Of56 y | Choice26Of56 z
    | Choice27Of56 aa | Choice28Of56 ab | Choice29Of56 ac | Choice30Of56 ad
    | Choice31Of56 ae | Choice32Of56 af | Choice33Of56 ag | Choice34Of56 ah
    | Choice35Of56 ai | Choice36Of56 aj | Choice37Of56 ak | Choice38Of56 al
    | Choice39Of56 am | Choice40Of56 an | Choice41Of56 ao | Choice42Of56 ap
    | Choice43Of56 aq | Choice44Of56 ar | Choice45Of56 as | Choice46Of56 at
    | Choice47Of56 au | Choice48Of56 av | Choice49Of56 aw | Choice50Of56 ax
    | Choice51Of56 ay | Choice52Of56 az | Choice53Of56 ba | Choice54Of56 bb
    | Choice55Of56 bc | Choice56Of56 bd
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd)
    => XmlContent (OneOf56 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd)
  where
    fromElem cs =
        (choice OneOf56 $ choice TwoOf56 $ choice ThreeOf56 $ choice FourOf56
        $ choice FiveOf56 $ choice SixOf56 $ choice SevenOf56
        $ choice EightOf56 $ choice NineOf56 $ choice TenOf56
        $ choice ElevenOf56 $ choice TwelveOf56 $ choice ThirteenOf56
        $ choice FourteenOf56 $ choice FifteenOf56 $ choice SixteenOf56
        $ choice SeventeenOf56 $ choice EighteenOf56 $ choice NineteenOf56
        $ choice TwentyOf56 $ choice Choice21Of56 $ choice Choice22Of56
        $ choice Choice23Of56 $ choice Choice24Of56 $ choice Choice25Of56
        $ choice Choice26Of56 $ choice Choice27Of56 $ choice Choice28Of56
        $ choice Choice29Of56 $ choice Choice30Of56 $ choice Choice31Of56
        $ choice Choice32Of56 $ choice Choice33Of56 $ choice Choice34Of56
        $ choice Choice35Of56 $ choice Choice36Of56 $ choice Choice37Of56
        $ choice Choice38Of56 $ choice Choice39Of56 $ choice Choice40Of56
        $ choice Choice41Of56 $ choice Choice42Of56 $ choice Choice43Of56
        $ choice Choice44Of56 $ choice Choice45Of56 $ choice Choice46Of56
        $ choice Choice47Of56 $ choice Choice48Of56 $ choice Choice49Of56
        $ choice Choice50Of56 $ choice Choice51Of56 $ choice Choice52Of56
        $ choice Choice53Of56 $ choice Choice54Of56 $ choice Choice55Of56
        $ choice Choice56Of56
        $ (\c->(Nothing,c))) cs
    toElem (OneOf56 x) = toElem x
    toElem (TwoOf56 x) = toElem x
    toElem (ThreeOf56 x) = toElem x
    toElem (FourOf56 x) = toElem x
    toElem (FiveOf56 x) = toElem x
    toElem (SixOf56 x) = toElem x
    toElem (SevenOf56 x) = toElem x
    toElem (EightOf56 x) = toElem x
    toElem (NineOf56 x) = toElem x
    toElem (TenOf56 x) = toElem x
    toElem (ElevenOf56 x) = toElem x
    toElem (TwelveOf56 x) = toElem x
    toElem (ThirteenOf56 x) = toElem x
    toElem (FourteenOf56 x) = toElem x
    toElem (FifteenOf56 x) = toElem x
    toElem (SixteenOf56 x) = toElem x
    toElem (SeventeenOf56 x) = toElem x
    toElem (EighteenOf56 x) = toElem x
    toElem (NineteenOf56 x) = toElem x
    toElem (TwentyOf56 x) = toElem x
    toElem (Choice21Of56 x) = toElem x
    toElem (Choice22Of56 x) = toElem x
    toElem (Choice23Of56 x) = toElem x
    toElem (Choice24Of56 x) = toElem x
    toElem (Choice25Of56 x) = toElem x
    toElem (Choice26Of56 x) = toElem x
    toElem (Choice27Of56 x) = toElem x
    toElem (Choice28Of56 x) = toElem x
    toElem (Choice29Of56 x) = toElem x
    toElem (Choice30Of56 x) = toElem x
    toElem (Choice31Of56 x) = toElem x
    toElem (Choice32Of56 x) = toElem x
    toElem (Choice33Of56 x) = toElem x
    toElem (Choice34Of56 x) = toElem x
    toElem (Choice35Of56 x) = toElem x
    toElem (Choice36Of56 x) = toElem x
    toElem (Choice37Of56 x) = toElem x
    toElem (Choice38Of56 x) = toElem x
    toElem (Choice39Of56 x) = toElem x
    toElem (Choice40Of56 x) = toElem x
    toElem (Choice41Of56 x) = toElem x
    toElem (Choice42Of56 x) = toElem x
    toElem (Choice43Of56 x) = toElem x
    toElem (Choice44Of56 x) = toElem x
    toElem (Choice45Of56 x) = toElem x
    toElem (Choice46Of56 x) = toElem x
    toElem (Choice47Of56 x) = toElem x
    toElem (Choice48Of56 x) = toElem x
    toElem (Choice49Of56 x) = toElem x
    toElem (Choice50Of56 x) = toElem x
    toElem (Choice51Of56 x) = toElem x
    toElem (Choice52Of56 x) = toElem x
    toElem (Choice53Of56 x) = toElem x
    toElem (Choice54Of56 x) = toElem x
    toElem (Choice55Of56 x) = toElem x
    toElem (Choice56Of56 x) = toElem x

----
data OneOf57 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be
    = OneOf57 a | TwoOf57 b | ThreeOf57 c | FourOf57 d | FiveOf57 e
    | SixOf57 f | SevenOf57 g | EightOf57 h | NineOf57 i | TenOf57 j
    | ElevenOf57 k | TwelveOf57 l | ThirteenOf57 m | FourteenOf57 n
    | FifteenOf57 o | SixteenOf57 p | SeventeenOf57 q | EighteenOf57 r
    | NineteenOf57 s | TwentyOf57 t | Choice21Of57 u | Choice22Of57 v
    | Choice23Of57 w | Choice24Of57 x | Choice25Of57 y | Choice26Of57 z
    | Choice27Of57 aa | Choice28Of57 ab | Choice29Of57 ac | Choice30Of57 ad
    | Choice31Of57 ae | Choice32Of57 af | Choice33Of57 ag | Choice34Of57 ah
    | Choice35Of57 ai | Choice36Of57 aj | Choice37Of57 ak | Choice38Of57 al
    | Choice39Of57 am | Choice40Of57 an | Choice41Of57 ao | Choice42Of57 ap
    | Choice43Of57 aq | Choice44Of57 ar | Choice45Of57 as | Choice46Of57 at
    | Choice47Of57 au | Choice48Of57 av | Choice49Of57 aw | Choice50Of57 ax
    | Choice51Of57 ay | Choice52Of57 az | Choice53Of57 ba | Choice54Of57 bb
    | Choice55Of57 bc | Choice56Of57 bd | Choice57Of57 be
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be)
    => XmlContent (OneOf57 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be)
  where
    fromElem cs =
        (choice OneOf57 $ choice TwoOf57 $ choice ThreeOf57 $ choice FourOf57
        $ choice FiveOf57 $ choice SixOf57 $ choice SevenOf57
        $ choice EightOf57 $ choice NineOf57 $ choice TenOf57
        $ choice ElevenOf57 $ choice TwelveOf57 $ choice ThirteenOf57
        $ choice FourteenOf57 $ choice FifteenOf57 $ choice SixteenOf57
        $ choice SeventeenOf57 $ choice EighteenOf57 $ choice NineteenOf57
        $ choice TwentyOf57 $ choice Choice21Of57 $ choice Choice22Of57
        $ choice Choice23Of57 $ choice Choice24Of57 $ choice Choice25Of57
        $ choice Choice26Of57 $ choice Choice27Of57 $ choice Choice28Of57
        $ choice Choice29Of57 $ choice Choice30Of57 $ choice Choice31Of57
        $ choice Choice32Of57 $ choice Choice33Of57 $ choice Choice34Of57
        $ choice Choice35Of57 $ choice Choice36Of57 $ choice Choice37Of57
        $ choice Choice38Of57 $ choice Choice39Of57 $ choice Choice40Of57
        $ choice Choice41Of57 $ choice Choice42Of57 $ choice Choice43Of57
        $ choice Choice44Of57 $ choice Choice45Of57 $ choice Choice46Of57
        $ choice Choice47Of57 $ choice Choice48Of57 $ choice Choice49Of57
        $ choice Choice50Of57 $ choice Choice51Of57 $ choice Choice52Of57
        $ choice Choice53Of57 $ choice Choice54Of57 $ choice Choice55Of57
        $ choice Choice56Of57 $ choice Choice57Of57
        $ (\c->(Nothing,c))) cs
    toElem (OneOf57 x) = toElem x
    toElem (TwoOf57 x) = toElem x
    toElem (ThreeOf57 x) = toElem x
    toElem (FourOf57 x) = toElem x
    toElem (FiveOf57 x) = toElem x
    toElem (SixOf57 x) = toElem x
    toElem (SevenOf57 x) = toElem x
    toElem (EightOf57 x) = toElem x
    toElem (NineOf57 x) = toElem x
    toElem (TenOf57 x) = toElem x
    toElem (ElevenOf57 x) = toElem x
    toElem (TwelveOf57 x) = toElem x
    toElem (ThirteenOf57 x) = toElem x
    toElem (FourteenOf57 x) = toElem x
    toElem (FifteenOf57 x) = toElem x
    toElem (SixteenOf57 x) = toElem x
    toElem (SeventeenOf57 x) = toElem x
    toElem (EighteenOf57 x) = toElem x
    toElem (NineteenOf57 x) = toElem x
    toElem (TwentyOf57 x) = toElem x
    toElem (Choice21Of57 x) = toElem x
    toElem (Choice22Of57 x) = toElem x
    toElem (Choice23Of57 x) = toElem x
    toElem (Choice24Of57 x) = toElem x
    toElem (Choice25Of57 x) = toElem x
    toElem (Choice26Of57 x) = toElem x
    toElem (Choice27Of57 x) = toElem x
    toElem (Choice28Of57 x) = toElem x
    toElem (Choice29Of57 x) = toElem x
    toElem (Choice30Of57 x) = toElem x
    toElem (Choice31Of57 x) = toElem x
    toElem (Choice32Of57 x) = toElem x
    toElem (Choice33Of57 x) = toElem x
    toElem (Choice34Of57 x) = toElem x
    toElem (Choice35Of57 x) = toElem x
    toElem (Choice36Of57 x) = toElem x
    toElem (Choice37Of57 x) = toElem x
    toElem (Choice38Of57 x) = toElem x
    toElem (Choice39Of57 x) = toElem x
    toElem (Choice40Of57 x) = toElem x
    toElem (Choice41Of57 x) = toElem x
    toElem (Choice42Of57 x) = toElem x
    toElem (Choice43Of57 x) = toElem x
    toElem (Choice44Of57 x) = toElem x
    toElem (Choice45Of57 x) = toElem x
    toElem (Choice46Of57 x) = toElem x
    toElem (Choice47Of57 x) = toElem x
    toElem (Choice48Of57 x) = toElem x
    toElem (Choice49Of57 x) = toElem x
    toElem (Choice50Of57 x) = toElem x
    toElem (Choice51Of57 x) = toElem x
    toElem (Choice52Of57 x) = toElem x
    toElem (Choice53Of57 x) = toElem x
    toElem (Choice54Of57 x) = toElem x
    toElem (Choice55Of57 x) = toElem x
    toElem (Choice56Of57 x) = toElem x
    toElem (Choice57Of57 x) = toElem x

----
data OneOf58 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be bf
    = OneOf58 a | TwoOf58 b | ThreeOf58 c | FourOf58 d | FiveOf58 e
    | SixOf58 f | SevenOf58 g | EightOf58 h | NineOf58 i | TenOf58 j
    | ElevenOf58 k | TwelveOf58 l | ThirteenOf58 m | FourteenOf58 n
    | FifteenOf58 o | SixteenOf58 p | SeventeenOf58 q | EighteenOf58 r
    | NineteenOf58 s | TwentyOf58 t | Choice21Of58 u | Choice22Of58 v
    | Choice23Of58 w | Choice24Of58 x | Choice25Of58 y | Choice26Of58 z
    | Choice27Of58 aa | Choice28Of58 ab | Choice29Of58 ac | Choice30Of58 ad
    | Choice31Of58 ae | Choice32Of58 af | Choice33Of58 ag | Choice34Of58 ah
    | Choice35Of58 ai | Choice36Of58 aj | Choice37Of58 ak | Choice38Of58 al
    | Choice39Of58 am | Choice40Of58 an | Choice41Of58 ao | Choice42Of58 ap
    | Choice43Of58 aq | Choice44Of58 ar | Choice45Of58 as | Choice46Of58 at
    | Choice47Of58 au | Choice48Of58 av | Choice49Of58 aw | Choice50Of58 ax
    | Choice51Of58 ay | Choice52Of58 az | Choice53Of58 ba | Choice54Of58 bb
    | Choice55Of58 bc | Choice56Of58 bd | Choice57Of58 be | Choice58Of58 bf
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be
          ,XmlContent bf)
    => XmlContent (OneOf58 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be bf)
  where
    fromElem cs =
        (choice OneOf58 $ choice TwoOf58 $ choice ThreeOf58 $ choice FourOf58
        $ choice FiveOf58 $ choice SixOf58 $ choice SevenOf58
        $ choice EightOf58 $ choice NineOf58 $ choice TenOf58
        $ choice ElevenOf58 $ choice TwelveOf58 $ choice ThirteenOf58
        $ choice FourteenOf58 $ choice FifteenOf58 $ choice SixteenOf58
        $ choice SeventeenOf58 $ choice EighteenOf58 $ choice NineteenOf58
        $ choice TwentyOf58 $ choice Choice21Of58 $ choice Choice22Of58
        $ choice Choice23Of58 $ choice Choice24Of58 $ choice Choice25Of58
        $ choice Choice26Of58 $ choice Choice27Of58 $ choice Choice28Of58
        $ choice Choice29Of58 $ choice Choice30Of58 $ choice Choice31Of58
        $ choice Choice32Of58 $ choice Choice33Of58 $ choice Choice34Of58
        $ choice Choice35Of58 $ choice Choice36Of58 $ choice Choice37Of58
        $ choice Choice38Of58 $ choice Choice39Of58 $ choice Choice40Of58
        $ choice Choice41Of58 $ choice Choice42Of58 $ choice Choice43Of58
        $ choice Choice44Of58 $ choice Choice45Of58 $ choice Choice46Of58
        $ choice Choice47Of58 $ choice Choice48Of58 $ choice Choice49Of58
        $ choice Choice50Of58 $ choice Choice51Of58 $ choice Choice52Of58
        $ choice Choice53Of58 $ choice Choice54Of58 $ choice Choice55Of58
        $ choice Choice56Of58 $ choice Choice57Of58 $ choice Choice58Of58
        $ (\c->(Nothing,c))) cs
    toElem (OneOf58 x) = toElem x
    toElem (TwoOf58 x) = toElem x
    toElem (ThreeOf58 x) = toElem x
    toElem (FourOf58 x) = toElem x
    toElem (FiveOf58 x) = toElem x
    toElem (SixOf58 x) = toElem x
    toElem (SevenOf58 x) = toElem x
    toElem (EightOf58 x) = toElem x
    toElem (NineOf58 x) = toElem x
    toElem (TenOf58 x) = toElem x
    toElem (ElevenOf58 x) = toElem x
    toElem (TwelveOf58 x) = toElem x
    toElem (ThirteenOf58 x) = toElem x
    toElem (FourteenOf58 x) = toElem x
    toElem (FifteenOf58 x) = toElem x
    toElem (SixteenOf58 x) = toElem x
    toElem (SeventeenOf58 x) = toElem x
    toElem (EighteenOf58 x) = toElem x
    toElem (NineteenOf58 x) = toElem x
    toElem (TwentyOf58 x) = toElem x
    toElem (Choice21Of58 x) = toElem x
    toElem (Choice22Of58 x) = toElem x
    toElem (Choice23Of58 x) = toElem x
    toElem (Choice24Of58 x) = toElem x
    toElem (Choice25Of58 x) = toElem x
    toElem (Choice26Of58 x) = toElem x
    toElem (Choice27Of58 x) = toElem x
    toElem (Choice28Of58 x) = toElem x
    toElem (Choice29Of58 x) = toElem x
    toElem (Choice30Of58 x) = toElem x
    toElem (Choice31Of58 x) = toElem x
    toElem (Choice32Of58 x) = toElem x
    toElem (Choice33Of58 x) = toElem x
    toElem (Choice34Of58 x) = toElem x
    toElem (Choice35Of58 x) = toElem x
    toElem (Choice36Of58 x) = toElem x
    toElem (Choice37Of58 x) = toElem x
    toElem (Choice38Of58 x) = toElem x
    toElem (Choice39Of58 x) = toElem x
    toElem (Choice40Of58 x) = toElem x
    toElem (Choice41Of58 x) = toElem x
    toElem (Choice42Of58 x) = toElem x
    toElem (Choice43Of58 x) = toElem x
    toElem (Choice44Of58 x) = toElem x
    toElem (Choice45Of58 x) = toElem x
    toElem (Choice46Of58 x) = toElem x
    toElem (Choice47Of58 x) = toElem x
    toElem (Choice48Of58 x) = toElem x
    toElem (Choice49Of58 x) = toElem x
    toElem (Choice50Of58 x) = toElem x
    toElem (Choice51Of58 x) = toElem x
    toElem (Choice52Of58 x) = toElem x
    toElem (Choice53Of58 x) = toElem x
    toElem (Choice54Of58 x) = toElem x
    toElem (Choice55Of58 x) = toElem x
    toElem (Choice56Of58 x) = toElem x
    toElem (Choice57Of58 x) = toElem x
    toElem (Choice58Of58 x) = toElem x

----
data OneOf59 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be bf bg
    = OneOf59 a | TwoOf59 b | ThreeOf59 c | FourOf59 d | FiveOf59 e
    | SixOf59 f | SevenOf59 g | EightOf59 h | NineOf59 i | TenOf59 j
    | ElevenOf59 k | TwelveOf59 l | ThirteenOf59 m | FourteenOf59 n
    | FifteenOf59 o | SixteenOf59 p | SeventeenOf59 q | EighteenOf59 r
    | NineteenOf59 s | TwentyOf59 t | Choice21Of59 u | Choice22Of59 v
    | Choice23Of59 w | Choice24Of59 x | Choice25Of59 y | Choice26Of59 z
    | Choice27Of59 aa | Choice28Of59 ab | Choice29Of59 ac | Choice30Of59 ad
    | Choice31Of59 ae | Choice32Of59 af | Choice33Of59 ag | Choice34Of59 ah
    | Choice35Of59 ai | Choice36Of59 aj | Choice37Of59 ak | Choice38Of59 al
    | Choice39Of59 am | Choice40Of59 an | Choice41Of59 ao | Choice42Of59 ap
    | Choice43Of59 aq | Choice44Of59 ar | Choice45Of59 as | Choice46Of59 at
    | Choice47Of59 au | Choice48Of59 av | Choice49Of59 aw | Choice50Of59 ax
    | Choice51Of59 ay | Choice52Of59 az | Choice53Of59 ba | Choice54Of59 bb
    | Choice55Of59 bc | Choice56Of59 bd | Choice57Of59 be | Choice58Of59 bf
    | Choice59Of59 bg
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be
          ,XmlContent bf,XmlContent bg)
    => XmlContent (OneOf59 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be bf bg)
  where
    fromElem cs =
        (choice OneOf59 $ choice TwoOf59 $ choice ThreeOf59 $ choice FourOf59
        $ choice FiveOf59 $ choice SixOf59 $ choice SevenOf59
        $ choice EightOf59 $ choice NineOf59 $ choice TenOf59
        $ choice ElevenOf59 $ choice TwelveOf59 $ choice ThirteenOf59
        $ choice FourteenOf59 $ choice FifteenOf59 $ choice SixteenOf59
        $ choice SeventeenOf59 $ choice EighteenOf59 $ choice NineteenOf59
        $ choice TwentyOf59 $ choice Choice21Of59 $ choice Choice22Of59
        $ choice Choice23Of59 $ choice Choice24Of59 $ choice Choice25Of59
        $ choice Choice26Of59 $ choice Choice27Of59 $ choice Choice28Of59
        $ choice Choice29Of59 $ choice Choice30Of59 $ choice Choice31Of59
        $ choice Choice32Of59 $ choice Choice33Of59 $ choice Choice34Of59
        $ choice Choice35Of59 $ choice Choice36Of59 $ choice Choice37Of59
        $ choice Choice38Of59 $ choice Choice39Of59 $ choice Choice40Of59
        $ choice Choice41Of59 $ choice Choice42Of59 $ choice Choice43Of59
        $ choice Choice44Of59 $ choice Choice45Of59 $ choice Choice46Of59
        $ choice Choice47Of59 $ choice Choice48Of59 $ choice Choice49Of59
        $ choice Choice50Of59 $ choice Choice51Of59 $ choice Choice52Of59
        $ choice Choice53Of59 $ choice Choice54Of59 $ choice Choice55Of59
        $ choice Choice56Of59 $ choice Choice57Of59 $ choice Choice58Of59
        $ choice Choice59Of59
        $ (\c->(Nothing,c))) cs
    toElem (OneOf59 x) = toElem x
    toElem (TwoOf59 x) = toElem x
    toElem (ThreeOf59 x) = toElem x
    toElem (FourOf59 x) = toElem x
    toElem (FiveOf59 x) = toElem x
    toElem (SixOf59 x) = toElem x
    toElem (SevenOf59 x) = toElem x
    toElem (EightOf59 x) = toElem x
    toElem (NineOf59 x) = toElem x
    toElem (TenOf59 x) = toElem x
    toElem (ElevenOf59 x) = toElem x
    toElem (TwelveOf59 x) = toElem x
    toElem (ThirteenOf59 x) = toElem x
    toElem (FourteenOf59 x) = toElem x
    toElem (FifteenOf59 x) = toElem x
    toElem (SixteenOf59 x) = toElem x
    toElem (SeventeenOf59 x) = toElem x
    toElem (EighteenOf59 x) = toElem x
    toElem (NineteenOf59 x) = toElem x
    toElem (TwentyOf59 x) = toElem x
    toElem (Choice21Of59 x) = toElem x
    toElem (Choice22Of59 x) = toElem x
    toElem (Choice23Of59 x) = toElem x
    toElem (Choice24Of59 x) = toElem x
    toElem (Choice25Of59 x) = toElem x
    toElem (Choice26Of59 x) = toElem x
    toElem (Choice27Of59 x) = toElem x
    toElem (Choice28Of59 x) = toElem x
    toElem (Choice29Of59 x) = toElem x
    toElem (Choice30Of59 x) = toElem x
    toElem (Choice31Of59 x) = toElem x
    toElem (Choice32Of59 x) = toElem x
    toElem (Choice33Of59 x) = toElem x
    toElem (Choice34Of59 x) = toElem x
    toElem (Choice35Of59 x) = toElem x
    toElem (Choice36Of59 x) = toElem x
    toElem (Choice37Of59 x) = toElem x
    toElem (Choice38Of59 x) = toElem x
    toElem (Choice39Of59 x) = toElem x
    toElem (Choice40Of59 x) = toElem x
    toElem (Choice41Of59 x) = toElem x
    toElem (Choice42Of59 x) = toElem x
    toElem (Choice43Of59 x) = toElem x
    toElem (Choice44Of59 x) = toElem x
    toElem (Choice45Of59 x) = toElem x
    toElem (Choice46Of59 x) = toElem x
    toElem (Choice47Of59 x) = toElem x
    toElem (Choice48Of59 x) = toElem x
    toElem (Choice49Of59 x) = toElem x
    toElem (Choice50Of59 x) = toElem x
    toElem (Choice51Of59 x) = toElem x
    toElem (Choice52Of59 x) = toElem x
    toElem (Choice53Of59 x) = toElem x
    toElem (Choice54Of59 x) = toElem x
    toElem (Choice55Of59 x) = toElem x
    toElem (Choice56Of59 x) = toElem x
    toElem (Choice57Of59 x) = toElem x
    toElem (Choice58Of59 x) = toElem x
    toElem (Choice59Of59 x) = toElem x

----
data OneOf60 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be bf bg bh
    = OneOf60 a | TwoOf60 b | ThreeOf60 c | FourOf60 d | FiveOf60 e
    | SixOf60 f | SevenOf60 g | EightOf60 h | NineOf60 i | TenOf60 j
    | ElevenOf60 k | TwelveOf60 l | ThirteenOf60 m | FourteenOf60 n
    | FifteenOf60 o | SixteenOf60 p | SeventeenOf60 q | EighteenOf60 r
    | NineteenOf60 s | TwentyOf60 t | Choice21Of60 u | Choice22Of60 v
    | Choice23Of60 w | Choice24Of60 x | Choice25Of60 y | Choice26Of60 z
    | Choice27Of60 aa | Choice28Of60 ab | Choice29Of60 ac | Choice30Of60 ad
    | Choice31Of60 ae | Choice32Of60 af | Choice33Of60 ag | Choice34Of60 ah
    | Choice35Of60 ai | Choice36Of60 aj | Choice37Of60 ak | Choice38Of60 al
    | Choice39Of60 am | Choice40Of60 an | Choice41Of60 ao | Choice42Of60 ap
    | Choice43Of60 aq | Choice44Of60 ar | Choice45Of60 as | Choice46Of60 at
    | Choice47Of60 au | Choice48Of60 av | Choice49Of60 aw | Choice50Of60 ax
    | Choice51Of60 ay | Choice52Of60 az | Choice53Of60 ba | Choice54Of60 bb
    | Choice55Of60 bc | Choice56Of60 bd | Choice57Of60 be | Choice58Of60 bf
    | Choice59Of60 bg | Choice60Of60 bh
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be
          ,XmlContent bf,XmlContent bg,XmlContent bh)
    => XmlContent (OneOf60 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be bf bg
                           bh)
  where
    fromElem cs =
        (choice OneOf60 $ choice TwoOf60 $ choice ThreeOf60 $ choice FourOf60
        $ choice FiveOf60 $ choice SixOf60 $ choice SevenOf60
        $ choice EightOf60 $ choice NineOf60 $ choice TenOf60
        $ choice ElevenOf60 $ choice TwelveOf60 $ choice ThirteenOf60
        $ choice FourteenOf60 $ choice FifteenOf60 $ choice SixteenOf60
        $ choice SeventeenOf60 $ choice EighteenOf60 $ choice NineteenOf60
        $ choice TwentyOf60 $ choice Choice21Of60 $ choice Choice22Of60
        $ choice Choice23Of60 $ choice Choice24Of60 $ choice Choice25Of60
        $ choice Choice26Of60 $ choice Choice27Of60 $ choice Choice28Of60
        $ choice Choice29Of60 $ choice Choice30Of60 $ choice Choice31Of60
        $ choice Choice32Of60 $ choice Choice33Of60 $ choice Choice34Of60
        $ choice Choice35Of60 $ choice Choice36Of60 $ choice Choice37Of60
        $ choice Choice38Of60 $ choice Choice39Of60 $ choice Choice40Of60
        $ choice Choice41Of60 $ choice Choice42Of60 $ choice Choice43Of60
        $ choice Choice44Of60 $ choice Choice45Of60 $ choice Choice46Of60
        $ choice Choice47Of60 $ choice Choice48Of60 $ choice Choice49Of60
        $ choice Choice50Of60 $ choice Choice51Of60 $ choice Choice52Of60
        $ choice Choice53Of60 $ choice Choice54Of60 $ choice Choice55Of60
        $ choice Choice56Of60 $ choice Choice57Of60 $ choice Choice58Of60
        $ choice Choice59Of60 $ choice Choice60Of60
        $ (\c->(Nothing,c))) cs
    toElem (OneOf60 x) = toElem x
    toElem (TwoOf60 x) = toElem x
    toElem (ThreeOf60 x) = toElem x
    toElem (FourOf60 x) = toElem x
    toElem (FiveOf60 x) = toElem x
    toElem (SixOf60 x) = toElem x
    toElem (SevenOf60 x) = toElem x
    toElem (EightOf60 x) = toElem x
    toElem (NineOf60 x) = toElem x
    toElem (TenOf60 x) = toElem x
    toElem (ElevenOf60 x) = toElem x
    toElem (TwelveOf60 x) = toElem x
    toElem (ThirteenOf60 x) = toElem x
    toElem (FourteenOf60 x) = toElem x
    toElem (FifteenOf60 x) = toElem x
    toElem (SixteenOf60 x) = toElem x
    toElem (SeventeenOf60 x) = toElem x
    toElem (EighteenOf60 x) = toElem x
    toElem (NineteenOf60 x) = toElem x
    toElem (TwentyOf60 x) = toElem x
    toElem (Choice21Of60 x) = toElem x
    toElem (Choice22Of60 x) = toElem x
    toElem (Choice23Of60 x) = toElem x
    toElem (Choice24Of60 x) = toElem x
    toElem (Choice25Of60 x) = toElem x
    toElem (Choice26Of60 x) = toElem x
    toElem (Choice27Of60 x) = toElem x
    toElem (Choice28Of60 x) = toElem x
    toElem (Choice29Of60 x) = toElem x
    toElem (Choice30Of60 x) = toElem x
    toElem (Choice31Of60 x) = toElem x
    toElem (Choice32Of60 x) = toElem x
    toElem (Choice33Of60 x) = toElem x
    toElem (Choice34Of60 x) = toElem x
    toElem (Choice35Of60 x) = toElem x
    toElem (Choice36Of60 x) = toElem x
    toElem (Choice37Of60 x) = toElem x
    toElem (Choice38Of60 x) = toElem x
    toElem (Choice39Of60 x) = toElem x
    toElem (Choice40Of60 x) = toElem x
    toElem (Choice41Of60 x) = toElem x
    toElem (Choice42Of60 x) = toElem x
    toElem (Choice43Of60 x) = toElem x
    toElem (Choice44Of60 x) = toElem x
    toElem (Choice45Of60 x) = toElem x
    toElem (Choice46Of60 x) = toElem x
    toElem (Choice47Of60 x) = toElem x
    toElem (Choice48Of60 x) = toElem x
    toElem (Choice49Of60 x) = toElem x
    toElem (Choice50Of60 x) = toElem x
    toElem (Choice51Of60 x) = toElem x
    toElem (Choice52Of60 x) = toElem x
    toElem (Choice53Of60 x) = toElem x
    toElem (Choice54Of60 x) = toElem x
    toElem (Choice55Of60 x) = toElem x
    toElem (Choice56Of60 x) = toElem x
    toElem (Choice57Of60 x) = toElem x
    toElem (Choice58Of60 x) = toElem x
    toElem (Choice59Of60 x) = toElem x
    toElem (Choice60Of60 x) = toElem x

----
data OneOf61 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be bf bg bh bi
    = OneOf61 a | TwoOf61 b | ThreeOf61 c | FourOf61 d | FiveOf61 e
    | SixOf61 f | SevenOf61 g | EightOf61 h | NineOf61 i | TenOf61 j
    | ElevenOf61 k | TwelveOf61 l | ThirteenOf61 m | FourteenOf61 n
    | FifteenOf61 o | SixteenOf61 p | SeventeenOf61 q | EighteenOf61 r
    | NineteenOf61 s | TwentyOf61 t | Choice21Of61 u | Choice22Of61 v
    | Choice23Of61 w | Choice24Of61 x | Choice25Of61 y | Choice26Of61 z
    | Choice27Of61 aa | Choice28Of61 ab | Choice29Of61 ac | Choice30Of61 ad
    | Choice31Of61 ae | Choice32Of61 af | Choice33Of61 ag | Choice34Of61 ah
    | Choice35Of61 ai | Choice36Of61 aj | Choice37Of61 ak | Choice38Of61 al
    | Choice39Of61 am | Choice40Of61 an | Choice41Of61 ao | Choice42Of61 ap
    | Choice43Of61 aq | Choice44Of61 ar | Choice45Of61 as | Choice46Of61 at
    | Choice47Of61 au | Choice48Of61 av | Choice49Of61 aw | Choice50Of61 ax
    | Choice51Of61 ay | Choice52Of61 az | Choice53Of61 ba | Choice54Of61 bb
    | Choice55Of61 bc | Choice56Of61 bd | Choice57Of61 be | Choice58Of61 bf
    | Choice59Of61 bg | Choice60Of61 bh | Choice61Of61 bi
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be
          ,XmlContent bf,XmlContent bg,XmlContent bh,XmlContent bi)
    => XmlContent (OneOf61 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be bf bg
                           bh bi)
  where
    fromElem cs =
        (choice OneOf61 $ choice TwoOf61 $ choice ThreeOf61 $ choice FourOf61
        $ choice FiveOf61 $ choice SixOf61 $ choice SevenOf61
        $ choice EightOf61 $ choice NineOf61 $ choice TenOf61
        $ choice ElevenOf61 $ choice TwelveOf61 $ choice ThirteenOf61
        $ choice FourteenOf61 $ choice FifteenOf61 $ choice SixteenOf61
        $ choice SeventeenOf61 $ choice EighteenOf61 $ choice NineteenOf61
        $ choice TwentyOf61 $ choice Choice21Of61 $ choice Choice22Of61
        $ choice Choice23Of61 $ choice Choice24Of61 $ choice Choice25Of61
        $ choice Choice26Of61 $ choice Choice27Of61 $ choice Choice28Of61
        $ choice Choice29Of61 $ choice Choice30Of61 $ choice Choice31Of61
        $ choice Choice32Of61 $ choice Choice33Of61 $ choice Choice34Of61
        $ choice Choice35Of61 $ choice Choice36Of61 $ choice Choice37Of61
        $ choice Choice38Of61 $ choice Choice39Of61 $ choice Choice40Of61
        $ choice Choice41Of61 $ choice Choice42Of61 $ choice Choice43Of61
        $ choice Choice44Of61 $ choice Choice45Of61 $ choice Choice46Of61
        $ choice Choice47Of61 $ choice Choice48Of61 $ choice Choice49Of61
        $ choice Choice50Of61 $ choice Choice51Of61 $ choice Choice52Of61
        $ choice Choice53Of61 $ choice Choice54Of61 $ choice Choice55Of61
        $ choice Choice56Of61 $ choice Choice57Of61 $ choice Choice58Of61
        $ choice Choice59Of61 $ choice Choice60Of61 $ choice Choice61Of61
        $ (\c->(Nothing,c))) cs
    toElem (OneOf61 x) = toElem x
    toElem (TwoOf61 x) = toElem x
    toElem (ThreeOf61 x) = toElem x
    toElem (FourOf61 x) = toElem x
    toElem (FiveOf61 x) = toElem x
    toElem (SixOf61 x) = toElem x
    toElem (SevenOf61 x) = toElem x
    toElem (EightOf61 x) = toElem x
    toElem (NineOf61 x) = toElem x
    toElem (TenOf61 x) = toElem x
    toElem (ElevenOf61 x) = toElem x
    toElem (TwelveOf61 x) = toElem x
    toElem (ThirteenOf61 x) = toElem x
    toElem (FourteenOf61 x) = toElem x
    toElem (FifteenOf61 x) = toElem x
    toElem (SixteenOf61 x) = toElem x
    toElem (SeventeenOf61 x) = toElem x
    toElem (EighteenOf61 x) = toElem x
    toElem (NineteenOf61 x) = toElem x
    toElem (TwentyOf61 x) = toElem x
    toElem (Choice21Of61 x) = toElem x
    toElem (Choice22Of61 x) = toElem x
    toElem (Choice23Of61 x) = toElem x
    toElem (Choice24Of61 x) = toElem x
    toElem (Choice25Of61 x) = toElem x
    toElem (Choice26Of61 x) = toElem x
    toElem (Choice27Of61 x) = toElem x
    toElem (Choice28Of61 x) = toElem x
    toElem (Choice29Of61 x) = toElem x
    toElem (Choice30Of61 x) = toElem x
    toElem (Choice31Of61 x) = toElem x
    toElem (Choice32Of61 x) = toElem x
    toElem (Choice33Of61 x) = toElem x
    toElem (Choice34Of61 x) = toElem x
    toElem (Choice35Of61 x) = toElem x
    toElem (Choice36Of61 x) = toElem x
    toElem (Choice37Of61 x) = toElem x
    toElem (Choice38Of61 x) = toElem x
    toElem (Choice39Of61 x) = toElem x
    toElem (Choice40Of61 x) = toElem x
    toElem (Choice41Of61 x) = toElem x
    toElem (Choice42Of61 x) = toElem x
    toElem (Choice43Of61 x) = toElem x
    toElem (Choice44Of61 x) = toElem x
    toElem (Choice45Of61 x) = toElem x
    toElem (Choice46Of61 x) = toElem x
    toElem (Choice47Of61 x) = toElem x
    toElem (Choice48Of61 x) = toElem x
    toElem (Choice49Of61 x) = toElem x
    toElem (Choice50Of61 x) = toElem x
    toElem (Choice51Of61 x) = toElem x
    toElem (Choice52Of61 x) = toElem x
    toElem (Choice53Of61 x) = toElem x
    toElem (Choice54Of61 x) = toElem x
    toElem (Choice55Of61 x) = toElem x
    toElem (Choice56Of61 x) = toElem x
    toElem (Choice57Of61 x) = toElem x
    toElem (Choice58Of61 x) = toElem x
    toElem (Choice59Of61 x) = toElem x
    toElem (Choice60Of61 x) = toElem x
    toElem (Choice61Of61 x) = toElem x

----
data OneOf62 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be bf bg bh bi bj
    = OneOf62 a | TwoOf62 b | ThreeOf62 c | FourOf62 d | FiveOf62 e
    | SixOf62 f | SevenOf62 g | EightOf62 h | NineOf62 i | TenOf62 j
    | ElevenOf62 k | TwelveOf62 l | ThirteenOf62 m | FourteenOf62 n
    | FifteenOf62 o | SixteenOf62 p | SeventeenOf62 q | EighteenOf62 r
    | NineteenOf62 s | TwentyOf62 t | Choice21Of62 u | Choice22Of62 v
    | Choice23Of62 w | Choice24Of62 x | Choice25Of62 y | Choice26Of62 z
    | Choice27Of62 aa | Choice28Of62 ab | Choice29Of62 ac | Choice30Of62 ad
    | Choice31Of62 ae | Choice32Of62 af | Choice33Of62 ag | Choice34Of62 ah
    | Choice35Of62 ai | Choice36Of62 aj | Choice37Of62 ak | Choice38Of62 al
    | Choice39Of62 am | Choice40Of62 an | Choice41Of62 ao | Choice42Of62 ap
    | Choice43Of62 aq | Choice44Of62 ar | Choice45Of62 as | Choice46Of62 at
    | Choice47Of62 au | Choice48Of62 av | Choice49Of62 aw | Choice50Of62 ax
    | Choice51Of62 ay | Choice52Of62 az | Choice53Of62 ba | Choice54Of62 bb
    | Choice55Of62 bc | Choice56Of62 bd | Choice57Of62 be | Choice58Of62 bf
    | Choice59Of62 bg | Choice60Of62 bh | Choice61Of62 bi | Choice62Of62 bj
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be
          ,XmlContent bf,XmlContent bg,XmlContent bh,XmlContent bi
          ,XmlContent bj)
    => XmlContent (OneOf62 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be bf bg
                           bh bi bj)
  where
    fromElem cs =
        (choice OneOf62 $ choice TwoOf62 $ choice ThreeOf62 $ choice FourOf62
        $ choice FiveOf62 $ choice SixOf62 $ choice SevenOf62
        $ choice EightOf62 $ choice NineOf62 $ choice TenOf62
        $ choice ElevenOf62 $ choice TwelveOf62 $ choice ThirteenOf62
        $ choice FourteenOf62 $ choice FifteenOf62 $ choice SixteenOf62
        $ choice SeventeenOf62 $ choice EighteenOf62 $ choice NineteenOf62
        $ choice TwentyOf62 $ choice Choice21Of62 $ choice Choice22Of62
        $ choice Choice23Of62 $ choice Choice24Of62 $ choice Choice25Of62
        $ choice Choice26Of62 $ choice Choice27Of62 $ choice Choice28Of62
        $ choice Choice29Of62 $ choice Choice30Of62 $ choice Choice31Of62
        $ choice Choice32Of62 $ choice Choice33Of62 $ choice Choice34Of62
        $ choice Choice35Of62 $ choice Choice36Of62 $ choice Choice37Of62
        $ choice Choice38Of62 $ choice Choice39Of62 $ choice Choice40Of62
        $ choice Choice41Of62 $ choice Choice42Of62 $ choice Choice43Of62
        $ choice Choice44Of62 $ choice Choice45Of62 $ choice Choice46Of62
        $ choice Choice47Of62 $ choice Choice48Of62 $ choice Choice49Of62
        $ choice Choice50Of62 $ choice Choice51Of62 $ choice Choice52Of62
        $ choice Choice53Of62 $ choice Choice54Of62 $ choice Choice55Of62
        $ choice Choice56Of62 $ choice Choice57Of62 $ choice Choice58Of62
        $ choice Choice59Of62 $ choice Choice60Of62 $ choice Choice61Of62
        $ choice Choice62Of62
        $ (\c->(Nothing,c))) cs
    toElem (OneOf62 x) = toElem x
    toElem (TwoOf62 x) = toElem x
    toElem (ThreeOf62 x) = toElem x
    toElem (FourOf62 x) = toElem x
    toElem (FiveOf62 x) = toElem x
    toElem (SixOf62 x) = toElem x
    toElem (SevenOf62 x) = toElem x
    toElem (EightOf62 x) = toElem x
    toElem (NineOf62 x) = toElem x
    toElem (TenOf62 x) = toElem x
    toElem (ElevenOf62 x) = toElem x
    toElem (TwelveOf62 x) = toElem x
    toElem (ThirteenOf62 x) = toElem x
    toElem (FourteenOf62 x) = toElem x
    toElem (FifteenOf62 x) = toElem x
    toElem (SixteenOf62 x) = toElem x
    toElem (SeventeenOf62 x) = toElem x
    toElem (EighteenOf62 x) = toElem x
    toElem (NineteenOf62 x) = toElem x
    toElem (TwentyOf62 x) = toElem x
    toElem (Choice21Of62 x) = toElem x
    toElem (Choice22Of62 x) = toElem x
    toElem (Choice23Of62 x) = toElem x
    toElem (Choice24Of62 x) = toElem x
    toElem (Choice25Of62 x) = toElem x
    toElem (Choice26Of62 x) = toElem x
    toElem (Choice27Of62 x) = toElem x
    toElem (Choice28Of62 x) = toElem x
    toElem (Choice29Of62 x) = toElem x
    toElem (Choice30Of62 x) = toElem x
    toElem (Choice31Of62 x) = toElem x
    toElem (Choice32Of62 x) = toElem x
    toElem (Choice33Of62 x) = toElem x
    toElem (Choice34Of62 x) = toElem x
    toElem (Choice35Of62 x) = toElem x
    toElem (Choice36Of62 x) = toElem x
    toElem (Choice37Of62 x) = toElem x
    toElem (Choice38Of62 x) = toElem x
    toElem (Choice39Of62 x) = toElem x
    toElem (Choice40Of62 x) = toElem x
    toElem (Choice41Of62 x) = toElem x
    toElem (Choice42Of62 x) = toElem x
    toElem (Choice43Of62 x) = toElem x
    toElem (Choice44Of62 x) = toElem x
    toElem (Choice45Of62 x) = toElem x
    toElem (Choice46Of62 x) = toElem x
    toElem (Choice47Of62 x) = toElem x
    toElem (Choice48Of62 x) = toElem x
    toElem (Choice49Of62 x) = toElem x
    toElem (Choice50Of62 x) = toElem x
    toElem (Choice51Of62 x) = toElem x
    toElem (Choice52Of62 x) = toElem x
    toElem (Choice53Of62 x) = toElem x
    toElem (Choice54Of62 x) = toElem x
    toElem (Choice55Of62 x) = toElem x
    toElem (Choice56Of62 x) = toElem x
    toElem (Choice57Of62 x) = toElem x
    toElem (Choice58Of62 x) = toElem x
    toElem (Choice59Of62 x) = toElem x
    toElem (Choice60Of62 x) = toElem x
    toElem (Choice61Of62 x) = toElem x
    toElem (Choice62Of62 x) = toElem x

----
data OneOf63 a b c d e f g h i j k l m n o p q r s t u v w x y z aa ab ac ad
             ae af ag ah ai aj ak al am an ao ap aq ar as at au av aw ax ay
             az ba bb bc bd be bf bg bh bi bj bk
    = OneOf63 a | TwoOf63 b | ThreeOf63 c | FourOf63 d | FiveOf63 e
    | SixOf63 f | SevenOf63 g | EightOf63 h | NineOf63 i | TenOf63 j
    | ElevenOf63 k | TwelveOf63 l | ThirteenOf63 m | FourteenOf63 n
    | FifteenOf63 o | SixteenOf63 p | SeventeenOf63 q | EighteenOf63 r
    | NineteenOf63 s | TwentyOf63 t | Choice21Of63 u | Choice22Of63 v
    | Choice23Of63 w | Choice24Of63 x | Choice25Of63 y | Choice26Of63 z
    | Choice27Of63 aa | Choice28Of63 ab | Choice29Of63 ac | Choice30Of63 ad
    | Choice31Of63 ae | Choice32Of63 af | Choice33Of63 ag | Choice34Of63 ah
    | Choice35Of63 ai | Choice36Of63 aj | Choice37Of63 ak | Choice38Of63 al
    | Choice39Of63 am | Choice40Of63 an | Choice41Of63 ao | Choice42Of63 ap
    | Choice43Of63 aq | Choice44Of63 ar | Choice45Of63 as | Choice46Of63 at
    | Choice47Of63 au | Choice48Of63 av | Choice49Of63 aw | Choice50Of63 ax
    | Choice51Of63 ay | Choice52Of63 az | Choice53Of63 ba | Choice54Of63 bb
    | Choice55Of63 bc | Choice56Of63 bd | Choice57Of63 be | Choice58Of63 bf
    | Choice59Of63 bg | Choice60Of63 bh | Choice61Of63 bi | Choice62Of63 bj
    | Choice63Of63 bk
    deriving (Eq,Show)

instance (XmlContent a,XmlContent b,XmlContent c,XmlContent d,XmlContent e
          ,XmlContent f,XmlContent g,XmlContent h,XmlContent i,XmlContent j
          ,XmlContent k,XmlContent l,XmlContent m,XmlContent n,XmlContent o
          ,XmlContent p,XmlContent q,XmlContent r,XmlContent s,XmlContent t
          ,XmlContent u,XmlContent v,XmlContent w,XmlContent x,XmlContent y
          ,XmlContent z,XmlContent aa,XmlContent ab,XmlContent ac
          ,XmlContent ad,XmlContent ae,XmlContent af,XmlContent ag
          ,XmlContent ah,XmlContent ai,XmlContent aj,XmlContent ak
          ,XmlContent al,XmlContent am,XmlContent an,XmlContent ao
          ,XmlContent ap,XmlContent aq,XmlContent ar,XmlContent as
          ,XmlContent at,XmlContent au,XmlContent av,XmlContent aw
          ,XmlContent ax,XmlContent ay,XmlContent az,XmlContent ba
          ,XmlContent bb,XmlContent bc,XmlContent bd,XmlContent be
          ,XmlContent bf,XmlContent bg,XmlContent bh,XmlContent bi
          ,XmlContent bj,XmlContent bk)
    => XmlContent (OneOf63 a b c d e f g h i j k l m n o p q r s t u v w x y
                           z aa ab ac ad ae af ag ah ai aj ak al am an ao ap
                           aq ar as at au av aw ax ay az ba bb bc bd be bf bg
                           bh bi bj bk)
  where
    fromElem cs =
        (choice OneOf63 $ choice TwoOf63 $ choice ThreeOf63 $ choice FourOf63
        $ choice FiveOf63 $ choice SixOf63 $ choice SevenOf63
        $ choice EightOf63 $ choice NineOf63 $ choice TenOf63
        $ choice ElevenOf63 $ choice TwelveOf63 $ choice ThirteenOf63
        $ choice FourteenOf63 $ choice FifteenOf63 $ choice SixteenOf63
        $ choice SeventeenOf63 $ choice EighteenOf63 $ choice NineteenOf63
        $ choice TwentyOf63 $ choice Choice21Of63 $ choice Choice22Of63
        $ choice Choice23Of63 $ choice Choice24Of63 $ choice Choice25Of63
        $ choice Choice26Of63 $ choice Choice27Of63 $ choice Choice28Of63
        $ choice Choice29Of63 $ choice Choice30Of63 $ choice Choice31Of63
        $ choice Choice32Of63 $ choice Choice33Of63 $ choice Choice34Of63
        $ choice Choice35Of63 $ choice Choice36Of63 $ choice Choice37Of63
        $ choice Choice38Of63 $ choice Choice39Of63 $ choice Choice40Of63
        $ choice Choice41Of63 $ choice Choice42Of63 $ choice Choice43Of63
        $ choice Choice44Of63 $ choice Choice45Of63 $ choice Choice46Of63
        $ choice Choice47Of63 $ choice Choice48Of63 $ choice Choice49Of63
        $ choice Choice50Of63 $ choice Choice51Of63 $ choice Choice52Of63
        $ choice Choice53Of63 $ choice Choice54Of63 $ choice Choice55Of63
        $ choice Choice56Of63 $ choice Choice57Of63 $ choice Choice58Of63
        $ choice Choice59Of63 $ choice Choice60Of63 $ choice Choice61Of63
        $ choice Choice62Of63 $ choice Choice63Of63
        $ (\c->(Nothing,c))) cs
    toElem (OneOf63 x) = toElem x
    toElem (TwoOf63 x) = toElem x
    toElem (ThreeOf63 x) = toElem x
    toElem (FourOf63 x) = toElem x
    toElem (FiveOf63 x) = toElem x
    toElem (SixOf63 x) = toElem x
    toElem (SevenOf63 x) = toElem x
    toElem (EightOf63 x) = toElem x
    toElem (NineOf63 x) = toElem x
    toElem (TenOf63 x) = toElem x
    toElem (ElevenOf63 x) = toElem x
    toElem (TwelveOf63 x) = toElem x
    toElem (ThirteenOf63 x) = toElem x
    toElem (FourteenOf63 x) = toElem x
    toElem (FifteenOf63 x) = toElem x
    toElem (SixteenOf63 x) = toElem x
    toElem (SeventeenOf63 x) = toElem x
    toElem (EighteenOf63 x) = toElem x
    toElem (NineteenOf63 x) = toElem x
    toElem (TwentyOf63 x) = toElem x
    toElem (Choice21Of63 x) = toElem x
    toElem (Choice22Of63 x) = toElem x
    toElem (Choice23Of63 x) = toElem x
    toElem (Choice24Of63 x) = toElem x
    toElem (Choice25Of63 x) = toElem x
    toElem (Choice26Of63 x) = toElem x
    toElem (Choice27Of63 x) = toElem x
    toElem (Choice28Of63 x) = toElem x
    toElem (Choice29Of63 x) = toElem x
    toElem (Choice30Of63 x) = toElem x
    toElem (Choice31Of63 x) = toElem x
    toElem (Choice32Of63 x) = toElem x
    toElem (Choice33Of63 x) = toElem x
    toElem (Choice34Of63 x) = toElem x
    toElem (Choice35Of63 x) = toElem x
    toElem (Choice36Of63 x) = toElem x
    toElem (Choice37Of63 x) = toElem x
    toElem (Choice38Of63 x) = toElem x
    toElem (Choice39Of63 x) = toElem x
    toElem (Choice40Of63 x) = toElem x
    toElem (Choice41Of63 x) = toElem x
    toElem (Choice42Of63 x) = toElem x
    toElem (Choice43Of63 x) = toElem x
    toElem (Choice44Of63 x) = toElem x
    toElem (Choice45Of63 x) = toElem x
    toElem (Choice46Of63 x) = toElem x
    toElem (Choice47Of63 x) = toElem x
    toElem (Choice48Of63 x) = toElem x
    toElem (Choice49Of63 x) = toElem x
    toElem (Choice50Of63 x) = toElem x
    toElem (Choice51Of63 x) = toElem x
    toElem (Choice52Of63 x) = toElem x
    toElem (Choice53Of63 x) = toElem x
    toElem (Choice54Of63 x) = toElem x
    toElem (Choice55Of63 x) = toElem x
    toElem (Choice56Of63 x) = toElem x
    toElem (Choice57Of63 x) = toElem x
    toElem (Choice58Of63 x) = toElem x
    toElem (Choice59Of63 x) = toElem x
    toElem (Choice60Of63 x) = toElem x
    toElem (Choice61Of63 x) = toElem x
    toElem (Choice62Of63 x) = toElem x
    toElem (Choice63Of63 x) = toElem x

----
