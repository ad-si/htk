-- | -----------------------------------------------------------------------
-- -
-- - module: TkVariables
-- -
-- - author: ludi
-- -
-- - --------------------------------------------------------------------

module TkVariables (

  TkVariable(..),
  HasVariable(..),

  createTkVariable,
  readTkVariable,
  setTkVariable

) where

import Core
import Computation
import Object


-- -----------------------------------------------------------------------
-- type
-- -----------------------------------------------------------------------

newtype GUIValue a => TkVariable a = TkVariable ObjectID


-- -----------------------------------------------------------------------
-- creation
-- -----------------------------------------------------------------------

createTkVariable :: GUIValue a => a -> IO (TkVariable a)
createTkVariable val =
  do
    oid <- newObject
    execTclScript ["global v" ++ show oid,
                   "set v" ++ show oid ++ " " ++ show (toGUIValue val)]
    return (TkVariable oid)


-- -----------------------------------------------------------------------
-- reading and writing
-- -----------------------------------------------------------------------

readTkVariable :: GUIValue a => TkVariable a -> IO a
readTkVariable (TkVariable oid) =
  do
    str <- evalCmd ("global v" ++ show oid ++ "; set v" ++ show oid)
    creadTk str

setTkVariable :: GUIValue a => TkVariable a -> a -> IO ()
setTkVariable (TkVariable oid) val =
  execTclScript ["global v" ++ show oid,
                 "set v" ++ show oid ++ " " ++ show (toGUIValue val)]


-- -----------------------------------------------------------------------
-- HasVariable
-- -----------------------------------------------------------------------

class (GUIObject w {-, GUIValue v-}) => HasVariable w {- (TkVariable v)-} where
  variable :: TkVariable v -> Config w
  variable (TkVariable oid) w =
    cset w "variable" ("v" ++ show oid) >> return w
