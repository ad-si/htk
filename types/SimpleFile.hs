{- A SimpleFile is an attribute corresponding to the file-part of an
   object.  They are "simple" in that they do not themselves
   have attributes, and so are not themselves objects.
   However objects containing files will refer to them via an
   attribute which is a SimpleFile.
   -}
module SimpleFile(
   SimpleFile,
   newSimpleFile
   )