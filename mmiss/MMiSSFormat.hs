{- We define a datatype for the format of text representations of
   objects, and a form for reading values in it.

   Since it's very simple to do, we also define a function for converting
   an Element into text according to a format. -}
module MMiSSFormat(
   Format(..),
   formatForm, -- :: Form Format
   toExtension, -- :: Format -> String
   ) where

import Computation

import SimpleForm

data Format = LaTeX | XML deriving Show

formatForm :: Form Format
formatForm = newFormOptionMenu2 [("LaTeX",LaTeX),("XML",XML)]

toExtension :: Format -> String
toExtension LaTeX = "tex"
toExtension XML = "xml"