{- #########################################################################

MODULE        : GUIValue
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Defines the class of values that can be passed to Tk.
                Unfortunately, we cannot (in general) rely on the
                definition provided by Haskells Read and Write since
                some types (e.g booleans) require a different
                unparsing in Tk. However, we can rely on default
                definitions to inherit the semantics of read and write
                for most types.


   ######################################################################### -}


module GUIValue (
        Generator(..),

        GUIVALUE(..), 
        GUIValue(..), 

        RawData(..),

        TkCommand(..),

        fromGUIValueIO,
        creadTk,

        illegalGUIValue 
        ) where

import Computation(raise)
import Char
import Debug(debug)


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
                GUIVALUE HaskellTk (toTKS (show (show v)))
        maybeGUIValue (GUIVALUE HaskellTk s)     = 
                case [x | (x,t) <- reads (read (fromTKS s)), ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  
        maybeGUIValue (GUIVALUE Tk s)    = 
                case [x | (x,t) <- reads s, ("","") <- lex t] of
                        [x] -> Just x
                        _   -> Nothing  

        fromGUIValue val = case (maybeGUIValue val) of (Just a) -> a


fromGUIValueIO :: GUIValue a => GUIVALUE -> IO a
fromGUIValueIO v = 
        case maybeGUIValue v of
                Nothing  -> do {
                                debug ("NO PARSE: " ++ show v);
                                raise illegalGUIValue
                                }
                (Just a) -> return a


creadTk :: GUIValue a => String -> IO a
creadTk s = 
        case maybeGUIValue (GUIVALUE Tk (restoreNL s)) of
                Nothing ->  do {
                        print ("NO PARSE: " ++ s);
                        raise illegalGUIValue
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
        toGUIValue str = GUIVALUE HaskellTk (toTKS (show str))
        maybeGUIValue (GUIVALUE HaskellTk str) = Just (read (fromTKS str))
        maybeGUIValue (GUIVALUE Tk str) =  Just str      
                {- tk delivers raw, unquoted strings - great !! -}


-- --------------------------------------------------------------------------
-- [String]
-- --------------------------------------------------------------------------

instance GUIValue [[Char]] where
        cdefault = []
        toGUIValue l = GUIVALUE HaskellTk (toTKS (show (unlines l)))
        maybeGUIValue (GUIVALUE HaskellTk str) = Just (lines (read (fromTKS str)))
        maybeGUIValue (GUIVALUE Tk str) = Just (lines str)


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
-- Tk String Conversion
-- --------------------------------------------------------------------------

toTKS []                = []
-- toTKS ('\n':str)     = "\\n" ++ toTKS str
-- toTKS ('\t':str)     = "\\t" ++ toTKS str
toTKS ('[':str)         = "\\[" ++ toTKS str
toTKS (']':str)         = "\\]" ++ toTKS str
toTKS ('{':str)         = "\\{" ++ toTKS str
toTKS ('}':str)         = "\\}" ++ toTKS str
-- toTKS ('\\':str)     = "\\\\" ++ toTKS str
-- toTKS ('\"':str)     = "\\\"" ++ toTKS str
toTKS ('$':str)         = "\\$" ++ toTKS str
toTKS (x:str)           = x : toTKS str


fromTKS [] = []
-- fromTKS ('\"':str)   = fromTKS str
-- fromTKS ('\\':'\\':str)      = '\\' : fromTKS str
-- fromTKS ('\\':'n':str)       = '\n' : fromTKS str
-- fromTKS ('\\':'t':str)       = '\t' : fromTKS str
fromTKS ('\\':'[':str)  = '[' : fromTKS str
fromTKS ('\\':']':str)  = ']' : fromTKS str
fromTKS ('\\':'{':str)  = '{' : fromTKS str
fromTKS ('\\':'}':str)  = '}' : fromTKS str
fromTKS ('\\':'$':str)  = '$' : fromTKS str
-- fromTKS ('\\':'\"':str)      = '\"' : fromTKS str
fromTKS (x:str)         = x : fromTKS str



