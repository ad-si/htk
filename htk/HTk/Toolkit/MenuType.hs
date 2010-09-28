-- | This module contains 'MenuType' - a general abstract datatype for menus -
-- plus some map-like operations on it.
--
-- NBNBNB.  'MenuType' is also used by the graphs and daVinci stuff, which is
-- supposed to be independent of HTk.  So before making HTk-specific changes
-- to this datatype, please find some way of harmlessly ignoring them
-- (at best) for daVinci.
module HTk.Toolkit.MenuType(
   MenuPrim(..), -- the general type of menus.

   -- Map functions
   mapMenuPrim, -- :: (a -> b) -> MenuPrim c a -> MenuPrim c b
   mapMenuPrim', -- :: (c -> d) -> MenuPrim c a -> MenuPrim d a
   mapMMenuPrim, -- :: (Monad m) => (a -> m b) -> MenuPrim c a
      -- -> m (MenuPrim c b)
   mapMMenuPrim', -- :: (Monad m) => (c -> m d) -> MenuPrim c a
      -- -> m (MenuPrim d a)
   ) where

-- ----------------------------------------------------------------------
-- General basic Menu type.
-- For particular applications, for example HTk menus, it is recommended
-- that we wrap MenuPrim inside a newtype, for example
--    newtype HTkMenu value = HTkMenu (MenuPrim (Maybe String) value)
-- Different applications will need different values for both the
-- typevariables subMenuValue and value; for example daVinci uses a
-- value of subMenuValue which isn't Maybe String while it is compiling
-- menus.
-- ----------------------------------------------------------------------

data MenuPrim subMenuValue value =
      Button String value
      -- A button with the given (String) label
   |  Menu subMenuValue [MenuPrim subMenuValue value]
      -- A list of buttons (or further menus) inside a menu
   |  Blank
      -- A Blank can be used to separate groups of menu buttons in the
      -- same menu.

-- ----------------------------------------------------------------------
-- Map functions
-- There are 4 of these, for each possible answer to the two questions
-- (1) map or monadic map?
-- (2) map first type or second type?
-- ----------------------------------------------------------------------

mapMenuPrim :: (a -> b) -> MenuPrim c a -> MenuPrim c b
mapMenuPrim a2b (Button label a) = Button label (a2b a)
mapMenuPrim a2b (Menu subMenuValue menuButtons) =
   Menu subMenuValue (map (mapMenuPrim a2b) menuButtons)
mapMenuPrim a2b Blank = Blank

mapMenuPrim' :: (c -> d) -> MenuPrim c a -> MenuPrim d a
mapMenuPrim' c2d (Button title action) = Button title action
mapMenuPrim' c2d (Menu subMenuValue menuButtons) =
   Menu (c2d subMenuValue) (map (mapMenuPrim' c2d) menuButtons)
mapMenuPrim' c2d Blank = Blank

mapMMenuPrim :: (Monad m) => (a -> m b) -> MenuPrim c a
   -> m (MenuPrim c b)
mapMMenuPrim a2bAct (Button label a) =
   do
      b <- a2bAct a
      return (Button label b)
mapMMenuPrim a2bAct (Menu subMenuValue menuButtons) =
   do
      bMenuButtons <- mapM (mapMMenuPrim a2bAct) menuButtons
      return (Menu subMenuValue bMenuButtons)
mapMMenuPrim a2bAct Blank = return Blank

mapMMenuPrim' :: (Monad m) => (c -> m d) -> MenuPrim c a
   -> m (MenuPrim d a)
mapMMenuPrim' c2dAct (Button title action) =
   return (Button title action)
mapMMenuPrim' c2dAct (Menu subMenuValue menuButtons) =
   do
      dMenuButtons <- mapM (mapMMenuPrim' c2dAct) menuButtons
      dSubMenuValue <- c2dAct subMenuValue
      return (Menu dSubMenuValue dMenuButtons)
mapMMenuPrim' c2dAct Blank = return Blank

