{------------------------------------
MODULE        : MainUDrawOntology
AUTHOR        : Simon Drees,
                University of Bremen
DATE          : 2005
VERSION       : 1.0
DESCRIPTION   :
-------------------------------------}

module Main where

import qualified Graphics.UI.WX       as WX
import qualified OntoCore             as OntoCore

main :: IO ()
main =
  do WX.start OntoCore.gui