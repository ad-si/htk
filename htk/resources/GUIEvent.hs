{- #########################################################################

MODULE        : GUIEvent
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Event patterns and event information reported by Tk.


   ######################################################################### -}


module GUIEvent (
        EventPatternID,

        StandardEvent(..),
        EventInfo(..),

        EventPatterns(..),
        EventPattern(..),
        XEventType(..),
        Modifiers(..),
        Modifier(..),
        EventFieldDsgs(..),
        EventFieldDsg,

        GUIEventDesignator(..),

        parseInfo

        )
where

import InterActor(EventPatternID(..))
import GUIValue
import Char
import Dynamics
import ExtendedPrelude
import Geometry
import Debug(debug)
                

-- --------------------------------------------------------------------------
-- Event Sequence
-- --------------------------------------------------------------------------

data StandardEvent =
          ButtonClicked 
        | SliderScaled
        | WindowDestroyed
        | SaveYourself

data EventPatterns = EventPatterns [EventPattern]

data EventPattern = EventPattern [Modifier] XEventType

data XEventType =
          ButtonPress (Maybe Int)
        | ButtonRelease (Maybe Int)
        | Circulate
        | Colourmap
        | Configure
        | Destroy
        | Enter
        | Expose
        | FocusIn
        | FocusOut
        | Gravity
        | Keymap
        | KeyPress (Maybe String)
        | KeyRelease (Maybe String)
        | Motion
        | Leave
        | Map
        | Property
        | Reparent
        | Unmap
        | Visibility

data Modifiers = Modifiers [Modifier]

data Modifier =
          Control
        | Shift
        | Lock
        | Button1
        | Button2
        | Button3
        | Button4
        | Button5
        | Any
        | Double
        | Triple
        | Meta
        | Alt
        | Mod1
        | Mod2
        | Mod3
        | Mod4
        | Mod5
        deriving Show


-- --------------------------------------------------------------------------
-- Event Fields
-- --------------------------------------------------------------------------

data EventFieldDsgs = EventFieldDsgs [EventFieldDsg]

data EventFieldDsg =
          XFieldDsg 
        | YFieldDsg
        | WFieldDsg
        | HFieldDsg
        | KeySymDsg
        | ButtonNoDsg
        | CountsDsg
        | PlaceDsg


-- --------------------------------------------------------------------------
-- Event Info
-- --------------------------------------------------------------------------

data EventInfo = EventInfo {
     sliderpos    :: Double,
     xfield       :: Distance, 
     yfield       :: Distance,
     wfield       :: Distance,
     hfield       :: Distance,
     keysym       :: String,
     buttonno     :: Int,
     counts       :: Int
     }
 |   Modification GUIVALUE
 |   NoEventInfo

eventInfoT :: TyCon 
eventInfoT = mkTyCon "GUIEvent" "EventInfo"
instance Typeable EventInfo where
        typeOf t = mkTypeTag eventInfoT []

-- --------------------------------------------------------------------------
-- GUIEventDesignator 
-- --------------------------------------------------------------------------

class GUIEventDesignator e where
        toEventPattern :: e -> [EventPattern]
        toEventFields :: e -> [EventFieldDsg]
        toEventPatternID :: e -> EventPatternID
        toEventFields = fields . toEventPattern
        toEventPatternID = show . EventPatterns . toEventPattern

instance GUIEventDesignator [EventPattern] where
        toEventPattern = id

instance GUIEventDesignator XEventType where
        toEventPattern ex = [EventPattern [] ex]

instance GUIEventDesignator (Modifier,XEventType) where
        toEventPattern (m,ex) = [EventPattern [m] ex]

instance GUIEventDesignator (Modifier,[Char]) where
        toEventPattern (m,ex) = [EventPattern [m] (KeyPress (Just ex))]

instance GUIEventDesignator [Char] where
        toEventPattern key = [EventPattern [] (KeyPress (Just key))]

instance GUIEventDesignator ([Modifier],XEventType) where
        toEventPattern (ml,ex) = [EventPattern ml ex]

instance GUIEventDesignator StandardEvent where
        toEventPattern _ = []
        toEventFields _ = []
        toEventPatternID = show


-- --------------------------------------------------------------------------
-- Unparsing of Events 
-- --------------------------------------------------------------------------

instance Show StandardEvent where
   showsPrec d p r = 
      (case p of 
          ButtonClicked -> "CL"
          SliderScaled -> "SS"
          WindowDestroyed -> "WD"
          SaveYourself -> "SY"
        ) ++ r


instance Show EventPatterns where
   showsPrec d p r = 
      (case p of 
          (EventPatterns []) -> 
                ""
          (EventPatterns (ep:es)) -> 
                "<" ++ show ep ++ ">" ++ show (EventPatterns es)
        ) ++ r


instance Show EventPattern where
   showsPrec d p r = 
      (case p of 
          (EventPattern [] xev) -> show xev
          (EventPattern ml xev) -> show (Modifiers ml) ++ show xev
        ) ++ r

instance Show Modifiers where
   showsPrec d p r = 
      (case p of 
          (Modifiers []) -> 
                ""
          (Modifiers (m:ml)) -> 
                show m ++ "-" ++ show (Modifiers ml)
        ) ++ r


instance Show XEventType where
   showsPrec d p r = 
      (case p of 
        (ButtonPress Nothing) -> "ButtonPress"  
        (ButtonRelease Nothing) -> "ButtonRelease" 
        (ButtonPress (Just i)) -> "ButtonPress-" ++ show i 
        (ButtonRelease (Just i)) -> "ButtonRelease-" ++ show i 
        (KeyPress Nothing) -> "KeyPress"
        (KeyRelease Nothing) -> "KeyRelease"
        (KeyPress (Just s)) -> "KeyPress-" ++ s
        (KeyRelease (Just s)) -> "KeyRelease-" ++ s
        Circulate -> "Circulate"
        Colourmap -> "Colormap"
        Configure -> "Configure"
        Destroy -> "Destroy"
        Enter -> "Enter"
        Expose -> "Expose"
        FocusIn -> "FocusIn"
        FocusOut -> "FocusOut"
        Gravity -> "Gravity"
        Keymap -> "Keymap"
        Motion -> "Motion"
        Leave -> "Leave"
        Map -> "Map"
        Property -> "Property"
        Reparent -> "Reparent"
        Unmap -> "Unmap"
        Visibility -> "Visibility"
        ) ++ r

instance Show EventFieldDsg where
  showsPrec d p r = 
      (case p of 
          XFieldDsg -> "X%x"
          YFieldDsg -> "Y%y"
          WFieldDsg -> "W%w"
          HFieldDsg -> "H%h"
          KeySymDsg -> "K%K"
          ButtonNoDsg -> "B%b"
          CountsDsg -> "C%c"
          PlaceDsg -> "P%p"
        ) ++ r
        
instance Show EventFieldDsgs where
  showsPrec d (EventFieldDsgs []) r = 
        r
  showsPrec d (EventFieldDsgs (x:l)) r = 
        show x ++ " " ++ showsPrec d (EventFieldDsgs l) r


-- --------------------------------------------------------------------------
-- Event Fields 
-- --------------------------------------------------------------------------

fields :: [EventPattern] -> [EventFieldDsg]
fields [] = []
fields [EventPattern _ xev] = eventfields xev
fields (e:es) = fields es

eventfields :: XEventType -> [EventFieldDsg]
eventfields (ButtonPress _) = [XFieldDsg, YFieldDsg, ButtonNoDsg]
eventfields (ButtonRelease _) = [XFieldDsg, YFieldDsg, ButtonNoDsg]
eventfields Circulate = [PlaceDsg]
eventfields Colourmap = []
eventfields Configure = [HFieldDsg, WFieldDsg] 
eventfields Destroy = []
eventfields Enter = [XFieldDsg, YFieldDsg]
eventfields Expose = [CountsDsg,HFieldDsg, WFieldDsg]
eventfields FocusIn = []
eventfields FocusOut = []
eventfields Gravity = []
eventfields Keymap = []
eventfields (KeyPress _) = [XFieldDsg, YFieldDsg, KeySymDsg]
eventfields (KeyRelease _) = [XFieldDsg, YFieldDsg, KeySymDsg]
eventfields Motion = [XFieldDsg, YFieldDsg, ButtonNoDsg]
eventfields Leave = [XFieldDsg, YFieldDsg]
eventfields Map = []
eventfields Property = []
eventfields Reparent = []
eventfields Unmap = []
eventfields Visibility = []


-- --------------------------------------------------------------------------
-- Parsing Event Fields 
-- --------------------------------------------------------------------------

instance Read EventInfo where
   readsPrec p b = [(readinfo defInfo (split (== ' ') str),"")]
        where str = dropWhile (isSpace) b

readinfo info [] = info
readinfo info (('X':tl):l) = readinfo (info{xfield = read tl}) l
readinfo info (('Y':tl):l) = readinfo (info{yfield = read tl}) l
readinfo info (('W':tl):l) = readinfo (info{wfield = read tl}) l
readinfo info (('H':tl):l) = readinfo (info{hfield = read tl}) l
readinfo info (('K':tl):l) = readinfo (info{keysym = tl}) l
readinfo info (('B':tl):l) = readinfo (info{buttonno = (read tl)}) l
readinfo info (('C':tl):l) = readinfo (info{counts = read tl}) l
readinfo info (pos:l)      = readinfo (info{sliderpos = (read pos)}) l


defInfo = EventInfo 0.0 0 0 0 0 "" 0 0

parseInfo words = readinfo defInfo words
