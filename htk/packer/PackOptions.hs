-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

---
-- Packing options for the pack geometry manager.
module PackOptions (

  PackOption(..),
  SideSpec(..),              -- left, right, top, bottom
  FillSpec(..),
  showPackOptions

) where

import Resources
import Geometry


-- -----------------------------------------------------------------------
-- standard pack options
-- -----------------------------------------------------------------------

---
-- The <code>SideSpec</code> datatype.
data SideSpec = AtLeft | AtRight | AtTop | AtBottom

---
-- The <code>FillSpec</code> datatype.
data FillSpec = X | Y | Both | None

---
-- Internal.
instance Show SideSpec where
---
-- Internal.
  showsPrec d AtLeft r = "left" ++ r
  showsPrec d AtRight r = "right" ++ r
  showsPrec d AtTop r = "top" ++ r
  showsPrec d AtBottom r = "bottom" ++ r

---
-- Internal.
instance Show FillSpec where
---
-- Internal.
  showsPrec d X r = "x" ++ r
  showsPrec d Y r = "y" ++ r
  showsPrec d Both r = "both" ++ r
  showsPrec d None r = "none" ++ r

data PackOption =
    Side SideSpec
  | Fill FillSpec
  | Expand Toggle
  | IPadX Distance
  | IPadY Distance
  | PadX Distance
  | PadY Distance
  | Anchor Anchor

---
-- Internal.
instance Show PackOption where
---
-- Internal.
  showsPrec d (Side spec) r = "side " ++ show spec ++ r
  showsPrec d (Fill spec) r = "fill " ++ show spec ++ r
  showsPrec d (Expand t) r = "expand " ++ show t ++ r
  showsPrec d (IPadX i) r = "ipadx " ++ show i ++ r
  showsPrec d (IPadY i) r = "ipady " ++ show i ++ r
  showsPrec d (PadX i) r = "padx " ++ show i ++ r
  showsPrec d (PadY i) r = "pady " ++ show i ++ r
  showsPrec d (Anchor a) r = "anchor " ++ show a ++ r

---
-- Internal.
showPackOptions :: [PackOption] -> String
showPackOptions [] = ""
showPackOptions (opt : opts) =
  "-" ++ show opt ++ " " ++ showPackOptions opts
