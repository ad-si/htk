-- | Pack options for the grid geometry manager.
module HTk.Kernel.GridPackOptions (

  GridPackOption(..),
  StickyKind(..),
  showGridPackOptions

) where


-- -----------------------------------------------------------------------
-- grid pack options
-- -----------------------------------------------------------------------

-- | Various pack options of the grid geometry manager.
data GridPackOption =
    Column Int             -- ^ the column to pack the widget
  | Row Int                -- ^ the row to pack the widget
  | GridPos (Int, Int)     -- ^ row column and row to pack the widget
  | Sticky StickyKind      -- ^ pack widgets sticky to the grid (see type <code>StickyKind</code>)
  | Columnspan Int         -- ^ columnspan like HTML
  | Rowspan Int            -- ^ rowspan like HTML
  | GridPadX Int           -- ^ horizontal pad
  | GridPadY Int           -- ^ vertical pad
  | GridIPadX Int          -- ^ inner horizontal pad
  | GridIPadY Int          -- ^ inner vertical pad

-- | Internal.
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

-- | The @StickyKind@ datatype - sticky packing to the grid.
data StickyKind =
    N | S | E | W
  | NS | NE | NW | SE | SW | EW
  | NSE | NSW | NEW | SEW
  | NSEW

-- | Internal.
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

-- | Internal.
showGridPackOptions :: [GridPackOption] -> String
showGridPackOptions [] = ""
showGridPackOptions (opt : opts) =
  "-" ++ show opt ++ " " ++ showGridPackOptions opts
