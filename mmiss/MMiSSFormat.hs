{- We define a datatype for the format of text representations of
   objects, and a form for reading values in it.

   Since it's very simple to do, we also define a function for converting
   an Element into text according to a format. -}
module MMiSSFormat(
   Format(..),
   formatForm, -- :: Form Format
   ) where
#include "config.h"

import Computation

import SimpleForm

#if HAXMLINT
import Text.XML.HaXml.Types
#else
import XmlTypes
#endif

import LaTeXParser

data Format = LaTeX | XML

formatForm :: Form Format
formatForm = newFormOptionMenu2 [("LaTeX",LaTeX),("XML",XML)]



   