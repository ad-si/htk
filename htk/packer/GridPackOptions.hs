---
-- Packing options for the grid packer. More to come...
module GridPackOptions (

  GridPackOption(..),
  StickyKind(..),
  showGridPackOptions

) where


-- -----------------------------------------------------------------------
-- grid pack options
-- -----------------------------------------------------------------------

data GridPackOption =
    Column Int
  | Row Int
  | GridPos (Int, Int)
  | Sticky StickyKind
  | Columnspan Int
  | Rowspan Int
  | GridPadX Int
  | GridPadY Int
  | GridIPadX Int
  | GridIPadY Int

instance Show GridPackOption where
  showsPrec d (Row i) r = "row " ++ show i ++ r
  showsPrec d (Column i) r = "column " ++ show i ++ r
  showsPrec d (GridPos (i, j)) r =
    "column " ++ show i ++ " -row " ++ show j ++ r
  showsPrec d (Sticky kind) r = "sticky " ++ show kind ++ r
  showsPrec d (Columnspan i) r = "columnspan " ++ show i ++ r
  showsPrec d (Rowspan i) r = "rowspan " ++ show i ++ r
  showsPrec d (GridPadX i) r = "padx " ++ show i ++ r
  showsPrec d (GridPadY i) r = "pady " ++ show i ++ r
  showsPrec d (GridIPadX i) r = "ipadx " ++ show i ++ r
  showsPrec d (GridIPadY i) r = "ipady " ++ show i ++ r

data StickyKind =
    N | S | E | W
  | NS | NE | NW | SE | SW | EW
  | NSE | NSW | NEW | SEW
  | NSEW

instance Show StickyKind where
  showsPrec d N r = "n"
  showsPrec d S r = "s"
  showsPrec d E r = "e"
  showsPrec d W r = "w"
  showsPrec d NS r = "ns"
  showsPrec d NE r = "ne"
  showsPrec d NW r = "nw"
  showsPrec d SE r = "se"
  showsPrec d SW r = "sw"
  showsPrec d EW r = "ew"
  showsPrec d NSE r = "nse"
  showsPrec d NSW r = "nsw"
  showsPrec d NEW r = "new"
  showsPrec d SEW r = "sew"
  showsPrec d NSEW r = "nsew"

showGridPackOptions :: [GridPackOption] -> String
showGridPackOptions [] = ""
showGridPackOptions (opt : opts) =
  "-" ++ show opt ++ " " ++ showGridPackOptions opts
