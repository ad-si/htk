{- #######################################################################

MODULE        : ScrollBox
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Composite widget for packing a scrollable widget together
                with the relevant number of scrollbars!

   #################################################################### -}


module ScrollBox (

  ScrollBox(..),
  newScrollBox,

  getScrolledWidget,
  getScrollBars

) where

import HTk
import Core

-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

{- HasScroller a requirement removed. -}
data ScrollBox a = 
        ScrollBox {
                fScrollFrame    :: Frame, 
                fPadFrames      :: [Frame],
                fScrollBars     :: [ScrollBar],
                fScrolledWidget :: a
                }

                
-- -----------------------------------------------------------------------
-- constructor
-- -----------------------------------------------------------------------

newScrollBox :: (Widget wid, HasScroller wid, Container par) =>
                par -> (Frame -> IO wid) ->
                [Config (ScrollBox wid)] ->
                IO (ScrollBox wid, wid)
newScrollBox par wfun ol =
  do
    f <- newFrame par []
    w <- wfun f
    let sz = cm 0.4
        sz' = if scrollY then sz else 0         -- width of y scrollbar
        scrollY = (isWfOrientation w Vertical)
        scrollX = (isWfOrientation w Horizontal)
    fl <- newFrame f [width sz']
    pack fl [Fill Y, Side AtRight]
    (sf,sby) <-
      if scrollY then
        do
          sb <- newScrollBar fl [width sz, orient Vertical]
          pack sb [Expand On, Fill Y, Side AtTop]
          configure w [scrollbar Vertical sb]
          sf <- newFrame fl [width sz, height (cm 0.5)]
          pack sf [Side AtBottom]
          return ([sf],[sb])
      else
        return ([],[])
    sbx <- if scrollX then
             do
               sb <- newScrollBar f [width sz, orient Horizontal]
               pack sb [Side AtBottom, Fill X]
               configure w [scrollbar Horizontal sb]
               return [sb]
           else
             return []
    let sbox = (ScrollBox f (fl:sf) (sbx ++ sby) w)
    configure sbox ol
    pack w [Fill Both, Expand On]
    return (sbox, w)


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Eq (ScrollBox a) where 
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (ScrollBox a) where 
  toGUIObject (ScrollBox w _ _ _) = toGUIObject w
  cname _ = "ScrollBox"

instance Destroyable (ScrollBox a) where
  destroy   = destroy . toGUIObject

instance (Widget a, HasScroller a) => Widget (ScrollBox a) where
  cursor c sb = 
    do
      foreach (fPadFrames sb) (cursor c)
      cursor c (fScrollFrame sb)
      foreach (fScrollBars sb) (cursor c)
      cursor c (fScrolledWidget sb)
      return sb
        
instance (HasColour a,HasScroller a) => HasColour (ScrollBox a) where
  legalColourID _ _ = True
  setColour sb cid c = 
    do
      foreach (fPadFrames sb) (\f -> setColour f cid c)
      setColour (fScrollFrame sb) cid c
      foreach (fScrollBars sb) (\s -> setColour s cid c)
      return sb

instance HasBorder (ScrollBox a)

instance HasScroller a => HasScroller (ScrollBox a) where
  isWfOrientation (ScrollBox _ _ _ sw) axis = isWfOrientation sw axis
  scrollbar _ _ sb = return sb                            -- already done
  moveto axis (ScrollBox _ _ _ sw) fraction = moveto axis sw fraction
  scroll axis (ScrollBox _ _ _ sw) step unit = scroll axis sw step unit

instance Synchronized (ScrollBox a) where
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- selectors
-- -----------------------------------------------------------------------

getScrolledWidget :: (Widget a, HasScroller a) => ScrollBox a -> a
getScrolledWidget = fScrolledWidget

getScrollBars :: HasScroller a => ScrollBox a -> [ScrollBar]
getScrollBars = fScrollBars
