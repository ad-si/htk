{- #########################################################################

MODULE        : CanvasItemAux
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996 - modified 2000 (Joel Wright)
VERSION       : alpha
DESCRIPTION   : Canvas Items - Auxiliary functions


   ######################################################################### -}


module CanvasItemAux (
        Canvas,

        CanvasItem(..),

        createCanvasItem,

        itemGeo,
        getGeo,
        setGeo,

        itemWidth,
        getItemWidth,

        itemHeight,
        getItemHeight,

        itemSize,
        getItemSize,

        itemPosition,
        getItemPosition,

        itemPositionD2,
        getItemPositionD2,

        canvasitemMethods
        ) where

import Concurrency
import GUICore
import Canvas
import CanvasItem
import Debug(debug)


-- --------------------------------------------------------------------------
-- Geometry
-- --------------------------------------------------------------------------

itemGeo :: CanvasItem w => Geometry -> Config w
itemGeo (w,h,x,y) = coord [(x,y),(x+w,y+h)]

getGeo :: CanvasItem w => w -> IO Geometry
getGeo wd = getCoord wd >>= coordToGeo 
        
setGeo :: CanvasItem w => w -> Geometry -> IO w
setGeo wd g = configure wd [itemGeo g]

itemWidth :: CanvasItem w => Distance -> Config w
itemWidth d item = getGeo item >>= \(_,h,x,y) -> setGeo item (d,h,x,y)

getItemWidth :: CanvasItem w => w -> IO Distance
getItemWidth item = getGeo item >>= \ (w,_,_,_) -> return w 

itemHeight :: CanvasItem w => Distance -> Config w
itemHeight d item = getGeo item >>= \(w,_,x,y) -> setGeo item (w,d,x,y)

getItemHeight :: CanvasItem w => w -> IO Distance
getItemHeight item = getGeo item >>= \(w,h,x,y) -> return h 

itemSize :: CanvasItem w => Size -> Config w
itemSize (w,h) item = getGeo item >>= \(_,_,x,y) -> setGeo item (w,h,x,y)

getItemSize :: CanvasItem w => w -> IO (Distance,Distance)
getItemSize item = getGeo item >>= \(w,h,x,y) -> return (w,h)

itemPosition :: CanvasItem w => Position -> Config w
itemPosition (x,y) item = getGeo item >>= \(w,h,_,_) -> setGeo item (w,h,x,y)

getItemPosition :: CanvasItem w => w -> IO (Distance,Distance)
getItemPosition item = getGeo item >>= \(w,h,x,y) -> return (x,y)

itemPositionD2 :: CanvasItem w => Position -> Config w
itemPositionD2 p = coord [p]

getItemPositionD2 :: CanvasItem w => w -> IO (Distance,Distance)
getItemPositionD2 w = getCoord w >>= return . head


-- --------------------------------------------------------------------------
-- Auxiliary
-- --------------------------------------------------------------------------

createCanvasItem :: CanvasItem w => CanvasItemKind -> (GUIOBJECT -> w) -> 
                        [Config w] -> IO w
createCanvasItem kind wrap ol = do {
        w <- createGUIObject (CANVASITEM kind []) canvasitemMethods;
        ci <- return(wrap w);
        configure ci ((coord (defaultCoord ci)) :ol);
}


coordToGeo ((x1,y1) :(x2,y2) : tl) = return (x2-x1,y2-y1,x1,y1)
coordToGeo _ = raise (userError "illegal geometry specification")
                                                      
-- --------------------------------------------------------------------------
--  Canvas Item Methods
-- --------------------------------------------------------------------------

canvasitemMethods = 
        Methods 
                tkGetCanvasItemConfig
                tkSetCanvasItemConfigs
                tkCreateCanvasItem 
                tkPackCanvasItem
                tkDestroyCanvasItem 
                tkCleanupCanvasItem 
                tkBindCanvasItem
                tkUnbindCanvasItem


-- --------------------------------------------------------------------------
-- Unparsing of Commands
-- --------------------------------------------------------------------------

tkCreateCanvasItem :: ObjectKind -> ObjectName -> ObjectID -> [ConfigOption] -> TclScript
tkCreateCanvasItem (k @ (CANVASITEM _ cds)) (cinm @ (CanvasItemName cnm tid)) _ args =
   [declVar tid, " set " ++ vname ++ " [" ++ cmd ++ "] "]
   where vname = (drop 1 (show tid))
         cmd = show cnm ++ " create " ++ show k ++ " " ++ show (toGUIValue cds) ++ " "
               ++ showConfigs args
tkCreateCanvasItem _ _ _ _ = []


declVar :: CanvasTagOrID -> TclCmd
declVar tid = "global " ++ (drop 1 (show tid))
 

tkPackCanvasItem :: ObjectKind -> ObjectName -> ObjectName -> [ConfigOption] -> 
                ObjectID -> [Binding] -> [TclCmd]
tkPackCanvasItem _ _ name _ oid binds = (tkDeclBindings name oid binds)


tkGetCanvasItemConfig :: ObjectName -> ConfigID -> TclScript
tkGetCanvasItemConfig (CanvasItemName name tid) "coords" =
        [declVar tid, show name ++ " coords " ++ show tid] 
tkGetCanvasItemConfig (CanvasItemName name tid) cid =   
        [declVar tid, show name ++ " itemcget " ++ show tid ++ " -" ++ cid]
tkGetCanvasItemConfig _ _ = []


tkSetCanvasItemConfigs :: ObjectName -> [ConfigOption] -> TclScript
tkSetCanvasItemConfigs _ [] = []
tkSetCanvasItemConfigs (CanvasItemName name tid) [("coords",val)] =
        [declVar tid,show name ++ " coords " ++ show tid ++ " " ++ show val]    
tkSetCanvasItemConfigs (CanvasItemName name tid) args = 
        [declVar tid, 
        show name ++ " itemconfigure " ++ show tid ++ " " ++ showConfigs args
        ]
tkSetCanvasItemConfigs _ _ = []


tkBindCanvasItem :: ObjectName -> ObjectID -> Binding -> TclScript
tkBindCanvasItem (CanvasItemName name tid) (ObjectID no) (tkev,f) = 
        [declVar tid, 
        show name ++ " bind " ++ show tid ++ " " ++ tkev ++ 
        " {puts stdout {EV " ++ show no ++ " " ++ tkev ++ " " ++
        show f ++ " }; flush stdout}"
        ]
tkBindCanvasItem _ _ _ = []
{-# INLINE tkBindCanvasItem #-}


tkUnbindCanvasItem :: ObjectName -> ObjectID -> Binding -> TclScript
tkUnbindCanvasItem (CanvasItemName name tid) (ObjectID no) (tkev,_) = 
        [declVar tid, 
        show name ++ " bind " ++ show tid ++ " " ++ tkev ++ " {}"
        ]
tkUnbindCanvasItem _ _ _ = []
{-# INLINE tkUnbindCanvasItem #-}


tkDestroyCanvasItem :: ObjectID -> ObjectName -> TclScript
tkDestroyCanvasItem _ name @ (CanvasItemName _ tid) = 
        [show name ++ " delete " ++ show tid]
tkDestroyCanvasItem _ _ = []


tkCleanupCanvasItem :: ObjectID -> ObjectName -> TclScript
tkCleanupCanvasItem _ (CanvasItemName _ tid) = 
        [declVar tid, " unset " ++ (drop 1 (show tid))]
tkCleanupCanvasItem _ _ = []

