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
-- This module exports a common interface for named objects.
module Name (

  Name(..),
  createName,
  getFullName,
  getShortName

) where

---
-- The <code>Name</code> datatype.
data Name = Name { short :: Int -> String,
	           full  :: String }

shortdef :: String -> Int -> String
shortdef str i =
  if length str > i then take (length str + 2 - i) str ++ ".." else str

---
-- Creates a new name.
-- @param str     - the full name.
-- @return result - A name.
createName :: String -> Name
createName str = Name { short = shortdef str, full = str }

---
-- Gets the full name from a <code>Name</code> object.
getFullName :: Name -> String
getFullName nm = full nm

---
-- Gets a short name of the given length from a <code>Name</code> object.
getShortName :: Name -> Int -> String
getShortName nm i = short nm i
