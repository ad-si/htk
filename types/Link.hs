{- Links are references to objects in the repository, which are instances
   of HasCodedValue, and so can be used as attributes of other objects
   in the repository. -}
module Link(
   Link,  
      -- A Link x (should) point to an object of type x.
      -- It is the responsibility of the person reading the link
      -- to make sure that x has the right type, otherwise
      -- the resolve function will create trouble when we
      -- attempt to read the link in.
   resolve, -- :: View -> Link x -> IO (Versioned x)

   mkLink, -- :: View -> Versioned x -> IO (Link x)
   
   topLinks, -- View -> IO [Link x]
      -- Returns links to the top links for the whole repository.
   ) where

data Link x = Link