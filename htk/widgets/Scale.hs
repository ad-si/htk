{- #########################################################################

MODULE        : Scale
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Scale Widget

TO BE DONE    : coords command
                Identify command


   ######################################################################### -}


module Scale (
        ScaleValue(..),

        Scale,
        newScale,

        HasIncrement(..),

        digits, 
        getDigits,

        interval, 
        getInterval,
        intervalTo,
        getIntervalTo,
        intervalFrom,
        getIntervalFrom,

        Slider(..),
        HasSlider(..),

        bigIncrement,
        getBigIncrement,
        
        showValue,
        getShowValue

        ) where

import Concurrency
import GUICore
import Packer
import Slider
import Interaction()
import Debug(debug)

-- --------------------------------------------------------------------------
--  Scale Type
-- --------------------------------------------------------------------------

data Scale a = Scale GUIOBJECT (PVar Double) (MVar (Double -> IO a))

{- the position should really be part of the kind attribute of the GUIObject -}

-- --------------------------------------------------------------------------
--  Classes
-- --------------------------------------------------------------------------

class GUIValue a => ScaleValue a where
        toDouble :: a -> Double
        fromDouble :: Double -> a


instance ScaleValue Double where
        toDouble = id
        fromDouble = id

        
-- --------------------------------------------------------------------------
--  Scale Creation
-- --------------------------------------------------------------------------

newScale :: [Config (Scale Double)] -> IO (Scale Double)
newScale ol = do 
        wid <- createGUIObject SCALE scaleMethods
        pv <- newPVar 0
        mv <- newMVar return
        sc <- return (Scale wid pv mv)
        configure sc [interval (0,100)]
        configure sc ol



-- --------------------------------------------------------------------------
--  Configuration Options: Instantiations
-- --------------------------------------------------------------------------

instance Eq (Scale a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (Scale a) where 
        toGUIObject (Scale w _ _) = w
        cname _ = "Scale"

instance Destructible (Scale a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Scale a)

instance Widget (Scale a)

instance ChildWidget (Scale a)

instance Synchronized (Scale a) where
        synchronize w = synchronize (toGUIObject w)

instance HasBorder (Scale a)

instance HasColour (Scale a) where
        legalColourID w "background" = True
        legalColourID w "foreground" = True
        legalColourID w "activebackground" = True
        legalColourID w _ = False

instance HasEnable (Scale a)

instance HasFont (Scale a)

instance ScaleValue a => HasIncrement (Scale a) a where
        increment d w  = cset w "tickinterval" (toDouble d)
        getIncrement w = cget w "tickinterval" >>= return . fromDouble

instance HasOrientation (Scale a)

instance HasSize (Scale a) where
        height d w  = cset w "length" d
        getHeight w = tkcget w "length"

instance HasSlider (Scale a)

instance GUIValue v => HasText (Scale a) v where
        text s w  = cset w  "label" s
        getText w = cget w "label"

instance ScaleValue a => Variable Scale a where   -- slider actually
        setVar w @ (Scale wid var _) pos =  synchronize w (do {
                setTclVariable (tvarname wid) pos';      -- (show pos');
                setVar var pos';
                t <- isPackedWidget wid;
                unless t (emitGUIEvent wid SliderScaled (EventInfo{sliderpos = pos'}))
                }) where pos' = toDouble pos
        getVar w@(Scale wid var _) = synchronize w (do {
                pos <- getVar var;
                return (fromDouble pos)
                })
        withVar w f = synchronize w (do {v <- getVar w; f v}) 
        updVar w f = synchronize w (do {
                v <- getVar w;
                (v',r) <- f v;
                setVar w v';
                return r
                })


-- --------------------------------------------------------------------------
--  Events
-- --------------------------------------------------------------------------

instance ScaleValue a => Reactive Scale a where
        triggered w @ (Scale _ var mv) = 
                userinteraction w SliderScaled Request >>>= \info ->
                        synchronize w (do {
                                setVar var (sliderpos info);
                                f <- getVar mv;
                                f (sliderpos info)
                                })

instance ScaleValue a => HasTrigger Scale a where
        getTrigger = return . triggered

instance HasMapTrigger Scale where
        mapTrigger f (Scale wid st mv) = do 
                g <- getVar mv
                mv' <- newMVar (\x -> g x >>= f)
                return (Scale wid st mv')

instance ScaleValue a => HasCommand Scale Double a where
        command f sc@(Scale wid st mv) = do {setVar mv f; return sc}


-- --------------------------------------------------------------------------
-- Slider Position Variable
-- --------------------------------------------------------------------------

tvarname :: GUIOBJECT -> String
tvarname wid = "v" ++ ((show . getObjectNo) wid)


-- --------------------------------------------------------------------------
--  Scale Specific Config Configs
-- --------------------------------------------------------------------------

digits :: Int -> Config (Scale a)
digits d w = cset w "digits" d

getDigits :: Scale a -> IO Int
getDigits w = cget w "digits"

intervalTo :: ScaleValue a => a -> Config (Scale a)
intervalTo v w = cset w "to" (toDouble v)


getIntervalTo :: ScaleValue a => Scale a -> IO a
getIntervalTo w = cget w "to" >>= return . fromDouble


intervalFrom :: ScaleValue a => a -> Config (Scale a)
intervalFrom v w = cset w "from" (toDouble v)


getIntervalFrom :: ScaleValue a => Scale a -> IO a
getIntervalFrom w = cget w "from" >>= return . fromDouble


interval :: ScaleValue a => (a, a) -> Config (Scale a)
interval (b,e) w = 
        synchronize w (do{
                cset w "to" (toDouble b); 
                cset w "from" (toDouble e)
                })


getInterval :: ScaleValue a => Scale a -> IO (a,a)
getInterval w = 
        synchronize w (do {
                cget w "to" >>= \b ->
                cget w "from" >>= \e ->
                return (fromDouble b,fromDouble e)
                })


-- --------------------------------------------------------------------------
--  Slider Specific Config Configs
-- --------------------------------------------------------------------------

instance ScaleValue a => HasIncrement (Slider (Scale a)) a where
        increment d w   = cset w "resolution" (toDouble d) 
        getIncrement w  = cget w "resolution" >>= return . fromDouble


instance HasSize (Slider (Scale a)) where
        width d w       = cset w "width" d
        getWidth w      = tkcget w "width"
        height d w      = cset w "sliderlength" d
        getHeight w     = tkcget w "sliderlength"  


bigIncrement ::  ScaleValue a => a -> Config (Slider (Scale a))
bigIncrement d w = cset w "bigincrement" (toDouble d)


getBigIncrement :: ScaleValue a => (Slider (Scale a)) -> IO a
getBigIncrement w = cget w "bigincrement" >>= return . fromDouble


showValue :: Toggle -> Config (Slider (Scale a))
showValue d w = cset w "showvalue" d


getShowValue :: (Slider (Scale a)) -> IO Toggle
getShowValue w = cget w "showvalue"


-- --------------------------------------------------------------------------
-- Scale Methods
-- --------------------------------------------------------------------------

scaleMethods :: Methods
scaleMethods = defMethods {
        packCmd   = tkPackScale,
        bindCmd   = tkBindScale,
        unbindCmd = tkUnbindScale
        }

        
-- --------------------------------------------------------------------------
--  Tk Intrinsics
-- --------------------------------------------------------------------------

tkScaleCmd :: ObjectID -> TclCmd
tkScaleCmd (ObjectID i) = "Scaled " ++ show i
{-# INLINE tkScaleCmd #-}


tkPackScale  _ _ name opts oid binds = 
        ("pack " ++ (show name) ++ " " ++ (showConfigs opts)) :
         concatMap (tkBindScale name oid) binds

tkBindScale ::ObjectName -> ObjectID -> Binding -> TclScript
tkBindScale name oid (tkev,_) | tkev == show SliderScaled = 
        (csetCmd defMethods) name [("command",toGUIValue (TkCommand (tkScaleCmd oid)))] 
tkBindScale name oid ev = (bindCmd defMethods) name oid ev
        

tkUnbindScale :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindScale name oid (tkev,_) | (tkev == show SliderScaled) = 
        (csetCmd defMethods) name [("command",toGUIValue (TkCommand ""))]       
tkUnbindScale name oid ev = (unbindCmd defMethods) name oid ev

