-- | A simple container for a group of button widgets.
module HTk.Toolkit.SelectBox (

  SelectBox,
  newSelectBox,

  addButton,
  addSpace,

  getDefault,
  selectDefault

) where

import HTk.Toplevel.HTk
import HTk.Kernel.GUIObject
import HTk.Widgets.Space
import Reactor.ReferenceVariables

-- -----------------------------------------------------------------------
-- SelectBox type
-- -----------------------------------------------------------------------

-- | The @SelectBox@ datatype.
data SelectBox = SelectBox Box (Maybe (Frame,Int)) (Ref [Button])

type Elements = [Button]


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

-- | Constructs a new select box and returns a handler.
newSelectBox :: Container par =>
   par
   -- ^ the parent widget, which has to be a container widget.
   -> Maybe Int
   -- ^ the optional index of a default button.
   -> [Config SelectBox]
   -- ^ the list of configuration options for this select box.
   -> IO SelectBox
   -- ^ A select box.
newSelectBox par def@(Nothing) cnf =
  do
    b <- newHBox par []
    pack b [Expand On, Fill X]
    em <- newRef []
    configure (SelectBox b Nothing em) cnf
newSelectBox par def@(Just i) ol =
  do
    b <- newHBox par []
    pack b [Expand On, Fill X]
    em <- newRef []
    f <- newFrame b [relief Sunken, borderwidth 1]
    pack f []
    configure (SelectBox b (Just (f,i)) em) ol


-- -----------------------------------------------------------------------
-- SelectBox instances
-- -----------------------------------------------------------------------

-- | Internal.
instance Eq SelectBox where
  w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

-- | A select box can be destroyed.
instance Destroyable SelectBox where
  -- Destroys a select box.
  destroy = destroy . toGUIObject

-- | Internal.
instance GUIObject SelectBox where
  toGUIObject (SelectBox b _ e) = toGUIObject b
  cname _ = "SelectBox"

-- | A select box has a configureable foreground and background colour.
instance HasColour SelectBox where
  legalColourID = hasForeGroundColour

-- | A select box has standard widget properties
-- (concerning focus, cursor).
instance Widget SelectBox

-- | A select box has a configureable size.
instance HasSize SelectBox

-- | A select box has a configureable border.
instance HasBorder SelectBox

-- | A select box is a stateful widget, it can be enabled or disabled.
instance HasEnable SelectBox where
  -- Sets the select box\'es state.
  state st sb@(SelectBox b _ em) =
    synchronize sb (do
                      ibs <- getRef em
                      foreach ibs (\ib -> configure ib [state st])
                      return sb)
  -- Gets the select box\'es state.
  getState sb = do
                  b <- isEnabled sb
                  if b then return Normal else return Disabled
  -- @True@, if the select box is enabled, otherwise
  -- @False@.
  isEnabled sb@(SelectBox b _ em) =
    synchronize sb (do
                      ibs <- getRef em
                      sl <- sequence (map getState ibs)
                      return (foldr (||) False (map (/= Disabled) sl)))

-- | You can synchronize on a select box.
instance Synchronized SelectBox where
  -- Synchronizes on a select box.
  synchronize = synchronize . toGUIObject


-- -----------------------------------------------------------------------
-- selection
-- -----------------------------------------------------------------------

-- | Selects the default button of a select box.
selectDefault :: SelectBox
   -- ^ the concerned select box.
   -> IO ()
   -- ^ None.
selectDefault sb =
  do
    mbt <- getDefault sb
    incase mbt (\bt -> flash bt >> invoke bt)

-- | Gets the default button from a select box (if there is one).
getDefault :: SelectBox
   -- ^ the concerned select box.
   -> IO (Maybe Button)
   -- ^ The default button of the select box
   -- (if there is one).
getDefault sb@(SelectBox b Nothing em) = return Nothing
getDefault sb@(SelectBox b (Just (f,i)) em) =
  do
    bts <- getRef em
    return (Just (bts !! i))


-- -----------------------------------------------------------------------
-- elements
-- -----------------------------------------------------------------------

-- | Adds a space widget at the end of the select box.
addSpace :: SelectBox
   -- ^ the concerned select box.
   -> Distance
   -- ^ the width of the space widget.
   -> IO Space
   -- ^ A space widget.
addSpace sb@(SelectBox b _ em) dist =
  do
    s <- newSpace b dist [orient Horizontal]
    pack s []
    return s

-- | Adds a button widget at the end of the select box.
addButton :: SelectBox
   -- ^ the concerned select box.
   -> [Config Button]
   -- ^ the list of configuration options for the constructed
   -- button.
   -> [PackOption]
   -- ^ the list of pack options for the constructed button.
   -> IO Button
   -- ^ A button widget.
addButton sb@(SelectBox b Nothing em) cnf pcnf =
  synchronize sb (do
                    bt <- newButton b cnf
                    pack bt pcnf
                    changeRef em (\el -> el ++ [bt])
                    return bt)
addButton sb@(SelectBox b (Just (f,i)) em) cnf pcnf =
  synchronize sb (do
                    el <- getRef em
                    let is_default = (i == length el + 1)

                    bt <- if is_default then newButton f cnf
                          else newButton b cnf
                    (if is_default then
                       do
                         bt <- newButton f cnf
                         pack bt [Side AtLeft, PadX (cm 0.2),
                                  PadY (cm 0.1)]
                         pack f (pcnf ++ [Side AtLeft, PadX (cm 0.2),
                                          PadY (cm 0.1)])
                     else
                       do
                         bt <- newButton b cnf
                         pack bt (Side AtLeft : pcnf))
                    setRef em (el ++ [bt])
                    return bt)
