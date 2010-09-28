module HTk.Kernel.GUIObjectName (

  ObjectName(..),
  TextItemName(..),
  WidgetName(..),
  CanvasTagOrID(..),
  toWidgetName

) where

import Util.Object(ObjectID(..))
import HTk.Kernel.GUIValue


-- -----------------------------------------------------------------------
--  Object Name
-- -----------------------------------------------------------------------

data ObjectName =
          ObjectName String                             -- widget
        | MenuItemName ObjectName Int                   -- menu item
        | CanvasItemName ObjectName CanvasTagOrID       -- canvas item
        | TextPaneItemName ObjectName TextItemName      -- text item
        | NoteBookPageName ObjectID
        | LabelFrameName ObjectName ObjectID
        | PaneName ObjectID

data TextItemName =
          TextTagID ObjectID
        | TextItemPosition GUIVALUE             -- Point actually
        | EmbeddedWindowName ObjectName

data CanvasTagOrID = CanvasTagOrID ObjectID
                   | CanvasTagNot CanvasTagOrID
                   | CanvasTagAnd CanvasTagOrID CanvasTagOrID
                   | CanvasTagOr  CanvasTagOrID CanvasTagOrID
                   | CanvasTagXOr CanvasTagOrID CanvasTagOrID

toWidgetName :: ObjectName -> WidgetName
toWidgetName (ObjectName s) = WidgetName s
{-# INLINE toWidgetName #-}


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance Show ObjectName where
   showsPrec d p r =
      (case p of
         ObjectName s -> s
         MenuItemName s _ -> show s
         TextPaneItemName s _ -> show s
         CanvasItemName s _ -> show s
         NoteBookPageName oid ->
           "[global v" ++ show oid ++ ";set dummy $v" ++ show oid ++ "]"
         LabelFrameName _ oid ->
           "[global v" ++ show oid ++ ";set dummy $v" ++ show oid ++
           "]"
         PaneName oid ->
           "[global v" ++ show oid ++ ";set dummy $v" ++ show oid ++ "]") ++ r

instance Show TextItemName where
   showsPrec d p r =
      (case p of
                (TextTagID (ObjectID i)) -> "tag" ++ show i
                (TextItemPosition p) -> show p
                (EmbeddedWindowName p) -> show p
        ) ++ r


instance Show CanvasTagOrID where
   showsPrec d (CanvasTagOrID i) r = "$v" ++ show i ++ r
   showsPrec d (CanvasTagAnd t1 t2) r = abr $ show t1 ++ "&&" ++ show t2 ++ r
   showsPrec d (CanvasTagOr  t1 t2) r = abr $ show t1 ++ "||" ++ show t2 ++ r
   showsPrec d (CanvasTagXOr t1 t2) r = abr $ show t1 ++ "^" ++ show t2 ++ r
   showsPrec d (CanvasTagNot t1)    r = abr $ "!"++ show t1 ++ r

abr :: String -> String
abr s = "("++s++")"

-- -----------------------------------------------------------------------
-- widget path names
-- -----------------------------------------------------------------------

data WidgetName = WidgetName String


-- -----------------------------------------------------------------------
-- instances
-- -----------------------------------------------------------------------

instance GUIValue WidgetName where
  cdefault = WidgetName "."

instance Read WidgetName where
   readsPrec p b = [(WidgetName b,[])]

instance Show WidgetName where
   showsPrec d (WidgetName p) r =  p ++  r
