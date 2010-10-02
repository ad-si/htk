module HTk.Canvasitems.CanvasItemAux (

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

import HTk.Kernel.Core
import HTk.Kernel.Geometry
import HTk.Canvasitems.CanvasItem
import Util.Computation


-- -----------------------------------------------------------------------
-- geometry
-- -----------------------------------------------------------------------

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


-- -----------------------------------------------------------------------
-- auxiliary
-- -----------------------------------------------------------------------

createCanvasItem :: CanvasItem w => Canvas -> CanvasItemKind ->
                                    (GUIOBJECT -> w) -> [Config w] ->
                                    Coord -> IO w
createCanvasItem cnv kind wrap ol co =
  do
    w <- createGUIObject (toGUIObject cnv) (CANVASITEM kind co)
                         canvasitemMethods
    let ci = wrap w
    configure ci ol

coordToGeo ((x1,y1) :(x2,y2) : tl) = return (x2-x1,y2-y1,x1,y1)
coordToGeo _ = raise (userError "illegal geometry specification")


-- -----------------------------------------------------------------------
--  canvas item methods
-- -----------------------------------------------------------------------

canvasitemMethods :: Methods
canvasitemMethods = Methods tkGetCanvasItemConfig
                            tkSetCanvasItemConfigs
                            tkCreateCanvasItem
                            (packCmd voidMethods)
                            (gridCmd voidMethods)
                            tkDestroyCanvasItem
                            tkBindCanvasItem
                            tkUnbindCanvasItem
                            tkCleanupCanvasItem


-- -----------------------------------------------------------------------
-- unparsing of commands
-- -----------------------------------------------------------------------

tkCreateCanvasItem :: ObjectName -> ObjectKind -> ObjectName ->
                      ObjectID -> [ConfigOption] -> TclScript
tkCreateCanvasItem _ k@(CANVASITEM _ cds)
                   (cinm @ (CanvasItemName cnm tid)) _ args =
   declVar tid ++ [" set " ++ vname ++ " [" ++ cmd ++ "] "]
   where vname = (drop 1 (show tid))
         cmd = show cnm ++ " create " ++ show k ++ " " ++
               show (toGUIValue cds) ++ " " ++ showConfigs args
{-
         cmd = show cnm ++ " create " ++ show k ++ " - coord " ++
               show (toGUIValue cds) ++ " " ++ showConfigs args
-}
tkCreateCanvasItem _ _ _ _ _ = error "CanvasItemAux (tkCreateCanvasItem)"

tkGetCanvasItemConfig :: ObjectName -> ConfigID -> TclScript
tkGetCanvasItemConfig (CanvasItemName name tid) "coords" =
  declVar tid ++ [show name ++ " coords " ++ show tid]
tkGetCanvasItemConfig (CanvasItemName name tid) cid =
  declVar tid ++ [show name ++ " itemcget " ++ show tid ++ " -" ++ cid]
tkGetCanvasItemConfig _ _ = []

tkSetCanvasItemConfigs (CanvasItemName name tid) args =
  declVar tid ++ tagVariables args ++
  [show name ++ " itemconfigure " ++ show tid ++ " " ++ showConfigs args]
  where tagVariables ((cid, cval) : ol) =
          case cid of
            "tag" -> ["global \"" ++ (drop 3 (show cval))] ++
                     tagVariables ol
            _     -> tagVariables ol
        tagVariables _                  = []
tkSetCanvasItemConfigs _ _ = []

tkDestroyCanvasItem :: ObjectName -> TclScript
tkDestroyCanvasItem name@(CanvasItemName _ tid) =
   declVar tid ++ [show name ++ " delete " ++ show tid]
tkDestroyCanvasItem _ = []

tkBindCanvasItem :: ObjectName -> BindTag -> [WishEvent] ->
                    EventInfoSet -> Bool -> TclScript
tkBindCanvasItem (CanvasItemName cnvnm cid) bindTag wishEvents
                 eventInfoSet _ =
  ["global " ++ drop 1 (show cid),
   show cnvnm ++ " bind " ++ show cid ++ " " ++
   delimitString (foldr (\ event soFar -> showP event soFar)
                        "" wishEvents) ++ " " ++
   mkBoundCmdArg bindTag eventInfoSet False]

tkUnbindCanvasItem :: ObjectName -> BindTag -> [WishEvent] -> Bool ->
                      TclScript
tkUnbindCanvasItem (CanvasItemName cnvnm cid) bindTag wishEvents _ = []

tkCleanupCanvasItem :: ObjectID -> ObjectName -> TclScript
tkCleanupCanvasItem _ (CanvasItemName _ tid) =
   declVar tid ++[" unset " ++ (drop 1 (show tid))]
tkCleanupCanvasItem _ _ = []
