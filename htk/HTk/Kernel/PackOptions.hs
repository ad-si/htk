-- | Packing options for the pack geometry manager.
module HTk.Kernel.PackOptions (

  PackOption(..),
  SideSpec(..),              -- left, right, top, bottom
  FillSpec(..),
  showPackOptions

) where

import HTk.Kernel.Resources
import HTk.Kernel.Geometry


-- -----------------------------------------------------------------------
-- standard pack options
-- -----------------------------------------------------------------------

-- | The @SideSpec@ datatype.
data SideSpec = AtLeft | AtRight | AtTop | AtBottom

-- | The @FillSpec@ datatype.
data FillSpec = X | Y | Both | None

-- | Internal.
instance Show SideSpec where
  -- Internal.
  showsPrec d AtLeft r = "left" ++ r
  showsPrec d AtRight r = "right" ++ r
  showsPrec d AtTop r = "top" ++ r
  showsPrec d AtBottom r = "bottom" ++ r

-- | Internal.
instance Show FillSpec where
  -- Internal.
  showsPrec d X r = "x" ++ r
  showsPrec d Y r = "y" ++ r
  showsPrec d Both r = "both" ++ r
  showsPrec d None r = "none" ++ r

data PackOption =
    Side SideSpec         -- ^ side to pack the widget
  | Fill FillSpec         -- ^ orientations to fill.
  | Expand Toggle         -- ^ expand toggle
  | IPadX Distance        -- ^ inner horizontal pad
  | IPadY Distance        -- ^ inner vertical pad
  | PadX Distance         -- ^ horizontal pad
  | PadY Distance         -- ^ vertical pad
  | Anchor Anchor         -- ^ anchor position

-- | Internal.
instance Show PackOption where
  -- Internal.
  showsPrec d (Side spec) r = "side " ++ show spec ++ r
  showsPrec d (Fill spec) r = "fill " ++ show spec ++ r
  showsPrec d (Expand t) r = "expand " ++ show t ++ r
  showsPrec d (IPadX i) r = "ipadx " ++ show i ++ r
  showsPrec d (IPadY i) r = "ipady " ++ show i ++ r
  showsPrec d (PadX i) r = "padx " ++ show i ++ r
  showsPrec d (PadY i) r = "pady " ++ show i ++ r
  showsPrec d (Anchor a) r = "anchor " ++ show a ++ r

-- | Internal.
showPackOptions :: [PackOption] -> String
showPackOptions [] = ""
showPackOptions (opt : opts) =
  "-" ++ show opt ++ " " ++ showPackOptions opts
