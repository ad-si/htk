-- | A simple scroll pane for a scrolled widget.
module HTk.Toolkit.ScrollBox (

  ScrollBox(..),
  newScrollBox,

  getScrolledWidget,
  getScrollBars

) where

import Util.Computation

import Events.Synchronized

import HTk.Toplevel.HTk
import HTk.Kernel.Core


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

-- | The @ScrollBox@ datatype.
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

-- | Constructs a new scrollbox and returns a handler.
newScrollBox :: (Widget wid, HasScroller wid, Container par) =>
   par
   -- ^ the parent widget, which has to be a container widget.
   -> (Frame -> IO wid)
   -- ^ a function that returns the scrollbox\'es content for
   -- a given parent container.
   ->
   [Config (ScrollBox wid)]
   -- ^ the list of configuration options for this scrollbox.
   ->
   IO (ScrollBox wid, wid)
   -- ^ A scrollbox.
newScrollBox par wfun cnf =
  do
    f <- newFrame par []
    w <- wfun f
    let sz = cm 0.3
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
    configure sbox cnf
    pack w [Fill Both, Expand On]
    return (sbox, w)


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq (ScrollBox a) where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | Internal.
instance GUIObject (ScrollBox a) where
  toGUIObject (ScrollBox w _ _ _) = toGUIObject w
  cname _ = "ScrollBox"

-- | A scrollbox can be destroyed.
instance Destroyable (ScrollBox a) where
  -- Destroys a scrollbox.
  destroy   = destroy . toGUIObject

-- | A scrollbox has standard widget properties
-- (concerning focus, cursor).
instance (Widget a, HasScroller a) => Widget (ScrollBox a) where
  cursor c sb =
    do
      foreach (fPadFrames sb) (cursor c)
      cursor c (fScrollFrame sb)
      foreach (fScrollBars sb) (cursor c)
      cursor c (fScrolledWidget sb)
      return sb

-- | A scrollbox has a configureable foreground and background colour.
instance (HasColour a,HasScroller a) => HasColour (ScrollBox a) where
  legalColourID _ _ = True
  setColour sb cid c =
    do
      foreach (fPadFrames sb) (\f -> setColour f cid c)
      setColour (fScrollFrame sb) cid c
      foreach (fScrollBars sb) (\s -> setColour s cid c)
      return sb

-- | A scrollbox has a configureable border.
instance HasBorder (ScrollBox a)

-- | A scrollbox has scrollbars.
instance HasScroller a => HasScroller (ScrollBox a) where
  isWfOrientation (ScrollBox _ _ _ sw) axis = isWfOrientation sw axis
  -- Dummy.
  scrollbar _ _ sb = return sb                            -- already done
  -- Moves the given axis to the given fraction.
  moveto axis (ScrollBox _ _ _ sw) fraction = moveto axis sw fraction
  -- Scrolls the given axis by the given amount.
  scroll axis (ScrollBox _ _ _ sw) step unit = scroll axis sw step unit

-- | You can synchronize on a scrollbox.
instance Synchronized (ScrollBox a) where
  -- Synchronizes on a scrollbox.
  synchronize = synchronize . toGUIObject

-- | A scrollbox has a configureable size.
instance HasSize (ScrollBox a) where
  -- Sets the width of the scrollbox.
  width w scb = fScrollFrame scb # width w >> return scb
  -- Sets the height of the scrollbox.
  height h scb = fScrollFrame scb # height h >> return scb


-- -----------------------------------------------------------------------
-- selectors
-- -----------------------------------------------------------------------

-- | Gets the scrolled widget from a scrollbox.
getScrolledWidget :: (Widget a, HasScroller a) => ScrollBox a -> a
getScrolledWidget = fScrolledWidget

-- | Gets the scrollbars from a scrollbox.
getScrollBars :: HasScroller a => ScrollBox a -> [ScrollBar]
getScrollBars = fScrollBars
