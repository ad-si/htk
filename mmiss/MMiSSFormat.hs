-- | We define a datatype for the format of text representations of
-- objects, and a form for reading values in it.
module MMiSSFormat(
   Format(..),
   formatForm, -- :: Form Format
   toExtension, -- :: Format -> String
   ) where

import SimpleForm

data Format = LaTeX | XML deriving Show

formatForm :: Form Format
formatForm = newFormOptionMenu2 [("LaTeX",LaTeX),("XML",XML)]

toExtension :: Format -> String
toExtension LaTeX = "tex"
toExtension XML = "xml"