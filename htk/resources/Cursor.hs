{- #########################################################################

MODULE        : Cursor
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Cursor specifications 


   ######################################################################### -}


module Cursor (
        GUIVALUE(..), 
        GUIValue(..), 
        Colour,

        CursorDesignator(..),

        Cursor(..),
        XCursor(..),
        BCursor(..), 

        arrow, 
        circle, 
        clock, 
        diamondCross, 
        dot, 
        drapedBox, 
        exchange, 
        fleur, 
        gobbler, 
        gumby, 
        hand1, 
        hand2, 
        pencil, 
        plus, 
        spraycan, 
        tcross, 
        watch
        
        ) where

import GUIValue
import Colour
import Char
import Debug(debug)


-- --------------------------------------------------------------------------
--  Cursor Type
-- --------------------------------------------------------------------------

newtype Cursor = Cursor String

data XCursor = XCursor String (Maybe Colour) (Maybe Colour)

data BCursor = BCursor String (Maybe String) Colour (Maybe Colour)


-- --------------------------------------------------------------------------
--  Cursor Handle
-- --------------------------------------------------------------------------

class CursorDesignator ch where
        toCursor :: ch -> Cursor

instance CursorDesignator Cursor where
        toCursor = id

instance  CursorDesignator XCursor where
        toCursor = Cursor . show

instance  CursorDesignator BCursor where
        toCursor = Cursor . show

instance CursorDesignator String where
        toCursor nm = toCursor (XCursor nm Nothing Nothing)

instance CursorDesignator (String,Colour) where
        toCursor (nm,fg) = 
                toCursor (XCursor nm (Just fg) Nothing)

instance CursorDesignator (String,Colour,Colour) where
        toCursor (nm,fg,bg) = 
                toCursor (XCursor nm (Just fg) (Just bg))

instance CursorDesignator ([Char],[Char],Colour,Colour) where
        toCursor (bfile,mfile,fg,bg) = 
                toCursor (BCursor bfile (Just mfile) fg (Just bg))


-- --------------------------------------------------------------------------
--  Standard X Cursors 
-- --------------------------------------------------------------------------

arrow, circle, clock, diamondCross, dot, drapedBox, exchange :: Cursor
fleur, gobbler, gumby, hand1, hand2, pencil, plus, spraycan :: Cursor
tcross, watch :: Cursor

arrow           = Cursor "arrow" 
circle          = Cursor "circle" 
clock           = Cursor "clock" 
diamondCross    = Cursor "diamondcross" 
dot             = Cursor "dot"
drapedBox       = Cursor "drapedbox" 
exchange        = Cursor "exchange"
fleur           = Cursor "fleur"
gobbler         = Cursor "gobbler" 
gumby           = Cursor "gumby" 
hand1           = Cursor "hand1" 
hand2           = Cursor "hand2" 
pencil          = Cursor "pencil" 
plus            = Cursor "plus" 
spraycan        = Cursor "spraycan" 
tcross          = Cursor "tcross" 
watch           = Cursor "watch"


-- --------------------------------------------------------------------------
--  Parsing/Unparsing 
-- --------------------------------------------------------------------------

instance GUIValue Cursor where
        cdefault = Cursor "xterm"

instance Read Cursor where
   readsPrec p b =
     case dropWhile (isSpace) b of
        ('{':xs) -> [(Cursor ("{" ++ (takeWhile (/= '}') xs) ++ "}"),"")]
        xs     -> [(Cursor (takeWhile (/= ' ') xs),"")]

instance Show Cursor where
   showsPrec d (Cursor p) r = p ++ r


-- --------------------------------------------------------------------------
--  XCursor 
-- --------------------------------------------------------------------------

instance Show XCursor where
   showsPrec d c r = cshow c ++ r
     where
        cshow (XCursor s Nothing Nothing) = s
        cshow (XCursor s (Just fg) Nothing) = 
                "{" ++ s ++ " " ++ show fg ++ "}"
        cshow (XCursor s (Just fg) (Just bg)) = 
                "{" ++ s ++ " " ++ show fg ++ " " ++ show bg ++ "}"


-- --------------------------------------------------------------------------
--  BCursor 
-- --------------------------------------------------------------------------

instance Show BCursor where
   showsPrec d c r = cshow c ++ r
     where
        cshow (BCursor fname Nothing fg Nothing) = 
                "{@" ++ fname ++ " " ++ show fg ++ "}"
        cshow (BCursor fname (Just bname) fg (Just bg)) = 
                "{" ++ fname ++ " " ++ bname ++ " " ++ show fg ++ 
                " " ++ show bg ++ "}"
