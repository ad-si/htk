{- #########################################################################

MODULE        : DaVinciRules
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : 


   ######################################################################### -}


module DaVinciRules (
        module DaVinciClasses,
        module DaVinciNodeType,
        module DaVinciEdgeType,

        TypeId(..),

        cleanupRuleBase,

        rulesTakePrecedence

        ) where

import Computation
import GUICore
import DaVinciCore
import DaVinciGraphTerm
import DaVinciClasses
import DaVinciNodeType
import DaVinciEdgeType


import Debug(debug)


-- ---------------------------------------------------------------------------
--  Commands for controlling the rule base
-- ---------------------------------------------------------------------------

cleanupRuleBase :: Graph -> IO ()
cleanupRuleBase g = do {withGraph g (return "visual(new_rules([]))"); done}


rulesTakePrecedence :: Bool -> Config Graph
rulesTakePrecedence False g = withGraph g (return "set(rules_first(false))")
rulesTakePrecedence True g = withGraph g (return "set(rules_first(true))")


