{- #########################################################################

MODULE        : ScrollBox
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Composite widget for packing a scrollable widget together
                with the relevant number of scrollbars!


   ######################################################################### -}


module ScrollBox (
        ScrollBox(..),
        newScrollBox,

        getScrolledWidget,
        getScrollBars

        ) where

import Concurrency
import HTk
import ScrollBar
import Frame
import Debug(debug)


                
-- --------------------------------------------------------------------------
--  Type
-- --------------------------------------------------------------------------

data HasScroller a => ScrollBox a = 
        ScrollBox {
                fScrollFrame    :: Frame, 
                fPadFrames      :: [Frame],
                fScrollBars     :: [ScrollBar],
                fScrolledWidget :: a
                }

                
-- --------------------------------------------------------------------------
--  Commands (Layout)
-- --------------------------------------------------------------------------

newScrollBox :: (ChildWidget a,HasScroller a)
          => a -> [Config (ScrollBox a)] -> IO (ScrollBox a)
newScrollBox w ol = do {
        f <- newFrame [];
        fl <- newFrame [width sz', side AtRight, fill Vertical,parent f];
        (sf,sby) <- if scrollY then do {
                        sb <- newScrollBar [
                                width sz, 
                                orient Vertical, 
                                expand On,
                                fill Vertical,
                                side AtTop,
                                parent fl];
                        configure w [scrollbar Vertical sb];
                        sf <- newFrame [ width sz, 
                                height (cm 0.5), 
                                side AtBottom, 
                                parent fl
                                ];
                        return ([sf],[sb])
                        }
                else 
                        return ([],[]);
        sbx <- if scrollX then do {
                        sb <- newScrollBar [
                                width sz, 
                                orient Horizontal, 
                                side AtBottom,
                                fill Horizontal,
                                parent f];
                        configure w [scrollbar Horizontal sb];
                        return [sb]
                        }
                else 
                        return [];
        configure w [fill Both, expand On, parent f];
        configure (ScrollBox f (fl:sf) (sbx ++ sby) w) ol
} where sz = cm 0.4
        sz' = if scrollY then sz else 0         -- width of y scrollbar
        scrollY = (isWfOrientation w Vertical)
        scrollX = (isWfOrientation w Horizontal)

                
-- --------------------------------------------------------------------------
--  Instances
-- --------------------------------------------------------------------------

instance Eq (ScrollBox a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance GUIObject (ScrollBox a) where 
        toGUIObject (ScrollBox w _ _ _) = toGUIObject w
        cname _ = "ScrollBox"

instance Destructible (ScrollBox a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (ScrollBox a)

instance (Widget a,HasScroller a) => Widget (ScrollBox a) where
        cursor c sb = 
                synchronize sb (do { 
                        foreach (fPadFrames sb) (cursor c);
                        cursor c (fScrollFrame sb); 
                        foreach (fScrollBars sb) (cursor c); 
                        cursor c (fScrolledWidget sb); 
                        return sb
                        })

instance (Widget a,HasScroller a) => ChildWidget (ScrollBox a)
        
instance (HasColour a,HasScroller a) => HasColour (ScrollBox a) where
        legalColourID _ _ = True
        setColour sb cid c = 
                synchronize sb (do {
                        foreach (fPadFrames sb) (\f -> setColour f cid c);
                        setColour (fScrollFrame sb) cid c; 
                        foreach (fScrollBars sb) (\s -> setColour s cid c); 
                        return sb
                        })

 
instance HasBorder (ScrollBox a)

instance HasScroller a => HasScroller (ScrollBox a) where
        isWfOrientation (ScrollBox _ _ _ sw) axis = isWfOrientation sw axis
        scrollbar _ _ sb = return sb                            -- already done
        moveto axis (ScrollBox _ _ _ sw) fraction = 
                moveto axis sw fraction
        scroll axis (ScrollBox _ _ _ sw) step unit =
                scroll axis sw step unit

instance Synchronized (ScrollBox a) where
        synchronize w = synchronize (toGUIObject w)
        

                
-- --------------------------------------------------------------------------
--  Selector
-- --------------------------------------------------------------------------

getScrolledWidget :: (Widget a, ChildWidget a,HasScroller a) => 
   ScrollBox a -> a
getScrolledWidget = fScrolledWidget

getScrollBars :: HasScroller a => ScrollBox a ->  [ScrollBar]
getScrollBars = fScrollBars

