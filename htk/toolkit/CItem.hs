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

module CItem (

  CItem(..)

) where

import HTk
import Name


------------------------------------------------------------
-- class CItem collects all properties items need to have --
------------------------------------------------------------

class Eq c => CItem c where
  getName :: c -> IO Name
  getIcon :: c -> IO Image
