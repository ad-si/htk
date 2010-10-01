{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module HTk.Kernel.GUIValue (
        Generator(..),

        GUIVALUE(..),
        GUIValue(..),

        RawData(..),

        TkCommand(..),

        creadTk,
        toTkString,
        escapeString,
        delimitString,

        illegalGUIValue
        ) where

import Data.Char
import Data.Maybe(isJust)
import Data.List (find)

-- --------------------------------------------------------------------------
--  Options
-- --------------------------------------------------------------------------

data GUIVALUE = GUIVALUE Generator String

data Generator = HaskellTk | Tk


-- --------------------------------------------------------------------------
--  Value Conversions
-- --------------------------------------------------------------------------

class (Show a, Read a) => GUIValue a where
        cdefault                        :: a
        toGUIValue                      :: a -> GUIVALUE
        maybeGUIValue                   :: GUIVALUE -> (Maybe a)
        fromGUIValue                    :: GUIVALUE -> a
        toGUIValue v                     =
                GUIVALUE HaskellTk (toTkString (show v))
        maybeGUIValue (GUIVALUE HaskellTk s)     =
                case [x | (x,t) <- reads (fromTkString s), ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing
        maybeGUIValue (GUIVALUE Tk s)    =
                case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing

        fromGUIValue val = case (maybeGUIValue val) of (Just a) -> a


creadTk :: GUIValue a => String -> IO a
creadTk s =
        case maybeGUIValue (GUIVALUE Tk (restoreNL s)) of
--                Nothing -> return cdefault
                Nothing ->  do {
                        print ("NO PARSE: " ++ s);
                        ioError illegalGUIValue
                        }
                (Just v) -> return v
        where   restoreNL [] = []
                restoreNL ('\\':'n':str) = '\n' : restoreNL str
                restoreNL (x:str)        = x : restoreNL str



illegalGUIValue :: IOError
illegalGUIValue = userError "illegal GUI value"


-- --------------------------------------------------------------------------
-- GUIVALUE
-- --------------------------------------------------------------------------

instance GUIValue GUIVALUE where
        cdefault        = GUIVALUE HaskellTk ""
        toGUIValue      = id
        maybeGUIValue   = Just . id

instance Read GUIVALUE where
   readsPrec p b =
     case b of
        xs -> [(GUIVALUE HaskellTk xs,[])]

instance Show GUIVALUE where
   showsPrec d (GUIVALUE _ p) r =  p ++  r



-- --------------------------------------------------------------------------
-- ()
-- --------------------------------------------------------------------------

instance GUIValue () where
        cdefault        = ()
        maybeGUIValue _ = Just ()


-- --------------------------------------------------------------------------
-- Raw Data
-- --------------------------------------------------------------------------

newtype RawData = RawData String

instance Read RawData where
        readsPrec p b = [(RawData b,[])]

instance Show RawData where
        showsPrec d (RawData p) r =  p ++  r

instance GUIValue RawData where
        cdefault = RawData ""


-- --------------------------------------------------------------------------
-- String
-- --------------------------------------------------------------------------

instance GUIValue [Char] where
        cdefault = []
        toGUIValue str = GUIVALUE HaskellTk (toTkString str)
        maybeGUIValue (GUIVALUE HaskellTk str) = Just (read (fromTkString str))
        maybeGUIValue (GUIVALUE Tk str) =  Just str
                {- tk delivers raw, unquoted strings - great !! -}


-- --------------------------------------------------------------------------
-- [String]
-- --------------------------------------------------------------------------

instance GUIValue [[Char]] where
        cdefault = []
        toGUIValue l = GUIVALUE HaskellTk (toTkString (unlines l))
        maybeGUIValue (GUIVALUE HaskellTk str) = Just (breakStr (read (fromTkString str)))
        maybeGUIValue (GUIVALUE Tk str) = Just (breakStr str)


-- Tk's lists lists of strings separated by blanks, but strings containing
-- blanks are enclosed by braces. Hence this:
breakStr :: String-> [String]
breakStr str = brk (dropWhile (' ' ==) str) where
  brk []     = [[]]
  brk (x:xs) = if x == '{' then let (c, r) = break ('}' ==) xs
                                in  c:brk (dropWhile ('}' ==) r)
                           else let (c, r) = break (' ' ==) xs
                                in  (x:c):brk (dropWhile (' ' ==) r)


-- --------------------------------------------------------------------------
-- Command
-- --------------------------------------------------------------------------

newtype TkCommand = TkCommand String

instance GUIValue TkCommand where
        cdefault = TkCommand "skip"
        toGUIValue c = GUIVALUE HaskellTk (show c)

instance Show TkCommand where
   showsPrec d (TkCommand s) r =  "{" ++ s ++ "}" ++  r

instance Read TkCommand where
   readsPrec p b  = [(TkCommand cmd,[])]
        where cmd = take (length b - 2) (drop 1 b)


-- --------------------------------------------------------------------------
-- Bool
-- --------------------------------------------------------------------------

instance GUIValue Bool where
        cdefault = False
        toGUIValue False                = GUIVALUE HaskellTk "0"
        toGUIValue True                 = GUIVALUE HaskellTk "1"
        maybeGUIValue (GUIVALUE _ "0")  = Just False
        maybeGUIValue (GUIVALUE _ "1")  = Just True
        maybeGUIValue (GUIVALUE _ _ )   = Nothing


-- --------------------------------------------------------------------------
-- Int
-- --------------------------------------------------------------------------

instance GUIValue Int where
        cdefault = 0


-- --------------------------------------------------------------------------
-- Double
-- --------------------------------------------------------------------------

instance GUIValue Double where
        cdefault = 0.0

-- --------------------------------------------------------------------------
-- Tuples
-- --------------------------------------------------------------------------

instance (GUIValue a, GUIValue b)=> GUIValue (a, b) where
        cdefault = (cdefault, cdefault)



-- --------------------------------------------------------------------------
-- Tk String Conversion
-- --------------------------------------------------------------------------

-- Conversion to Tk: escape and quote
toTkString :: String -> String
toTkString = quoteString . escapeString

-- escapeString quotes the special characters inside String.
escapeString :: String -> String
escapeString = concat . (map quoteChar)

-- quote places quotes around a String
quoteString :: String -> String
quoteString str = '\"':(str++"\"")

-- delimitString places quotes around a String, if it contains
-- spaces, making it possible to use it as a single argument.
delimitString :: String -> String
delimitString "" = "\"\""
delimitString str =
   if isJust (find isSpace str)
   then quoteString str else str



-- quoteChar quotes characters special to Tcl, but not %.
quoteChar :: Char -> String
quoteChar ch =
   case ch of
      '\\' -> "\\\\"
      '\"' -> "\\\""
      '\n' -> "\\n"
      '{' -> "\\{"
      '}' -> "\\}"
      '$' -> "\\$"
      '[' -> "\\["
      ']' -> "\\]"
      ';' -> "\\;"
      other ->
         if isPrint ch
            then
               [ch]
            else
               let
                  nchar = ord ch
               in
                  if (nchar<0 || nchar >=256)
                     then
                        error "TclSyntax: bad char"
                     else
                        let
                           (hi,lo) = nchar `divMod` 16
                        in
                           ['\\','x',intToDigit hi,intToDigit lo]


fromTkString :: String-> String

fromTkString [] = []
-- fromTkString ('\"':str)   = fromTkString str
fromTkString ('\\':'\\':str) = '\\' : fromTkString str
fromTkString ('\\':'n':str)  = '\n' : fromTkString str
fromTkString ('\\':'t':str)  = '\t' : fromTkString str
fromTkString ('\\':'[':str)  = '[' : fromTkString str
fromTkString ('\\':']':str)  = ']' : fromTkString str
fromTkString ('\\':'{':str)  = '{' : fromTkString str
fromTkString ('\\':'}':str)  = '}' : fromTkString str
fromTkString ('\\':'$':str)  = '$' : fromTkString str
fromTkString ('\\':';':str)  = ';' : fromTkString str
fromTkString ('\\':'\"':str) = '\"' : fromTkString str
fromTkString (x:str)         = x : fromTkString str

