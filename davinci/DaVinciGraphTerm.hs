{- #########################################################################

MODULE        : DaVinciGraphTerm
AUTHOR        : Carla Blanck Purper, 
                Einar W. Karlsen 
                University of Bremen
                email:  {cpurper,ewk}@informatik.uni-bremen.de
DATE          : 1996
VERSION       : beta
DESCRIPTION   : Definition of DaVinci Graph Terms.


   ######################################################################### -}


module DaVinciGraphTerm (
        GUIVALUE,

        AttrUpd(..),       
        AttrAssoc(..),     
        EdgeId(..),        
        EdgeDescr(..),     
        EdgeUpd(..),
        NodeId(..),        
        NodeDescr(..),     
        NodeUpd(..),
        TypeId(..)

        ) where

import GUIValue

import Debug(debug)




-- ---------------------------------------------------------------------------
-- Data Type Definitions
-- ---------------------------------------------------------------------------

data AttrUpd    = NodeAttrUpd NodeId [AttrAssoc]
                | EdgeAttrUpd EdgeId [AttrAssoc]

data AttrAssoc  = AttrAssoc String GUIVALUE 

newtype EdgeId  = EdgeId String deriving (Eq, Ord)

data EdgeDescr  = EdgeDescr EdgeId TypeId [AttrAssoc] NodeDescr

data EdgeUpd    = EdgeUpd EdgeId TypeId [AttrAssoc] NodeId NodeId
                | EdgeDel EdgeId

newtype NodeId  = NodeId String deriving (Eq, Ord)

data NodeDescr  = NodeDescr NodeId TypeId [AttrAssoc] [EdgeDescr]

data NodeUpd    = NodeUpd NodeId TypeId [AttrAssoc]
                | NodeDel NodeId

newtype TypeId  = TypeId String deriving (Eq, Ord)



-- ---------------------------------------------------------------------------
-- Unparsing Rules
-- ---------------------------------------------------------------------------

instance Show NodeId where
        showsPrec d (NodeId p) r = showsPrec d p r

instance Show EdgeId where
        showsPrec d (EdgeId p) r = showsPrec d p r

instance Show AttrUpd where
        showsPrec d (NodeAttrUpd nid ats) r = 
                "node("++ show nid ++ comma ++ show ats ++")" ++ r
        showsPrec d (EdgeAttrUpd eid ats) r =
                "edge("++ show eid ++ comma ++ show ats++")" ++ r

instance Show AttrAssoc where
        showsPrec d (AttrAssoc x y ) r = 
                "a("++ show x ++ comma ++ show y ++")" ++ r
        
instance Show EdgeDescr where
        showsPrec d (EdgeDescr eid tid ats et) r = 
                "l("++show eid ++comma++
                "e("++ show tid ++comma++ show ats ++comma++ show et ++"))" 
                ++ r

instance Show EdgeUpd where
        showsPrec d (EdgeUpd eid1 tid ats nid1 nid2) r =
                "new_edge("++ show eid1 ++comma++ show tid ++ comma ++
                        show ats ++ comma ++ show nid1 ++ comma ++
                        show nid2 ++")"
                        ++ r
        showsPrec d (EdgeDel eid1) r =
                "delete_edge("++show eid1++")" ++ r


instance Show NodeUpd where
        showsPrec d (NodeUpd nid tid ats) r =
                "new_node("++ show nid  ++comma++ show tid ++comma++ 
                show ats++")"
                ++ r
        showsPrec d (NodeDel nid) r = 
                "delete_node("++show nid++")" ++ r


instance Show NodeDescr where
        showsPrec d (NodeDescr nid tid ats ets) r =
                "l("++show nid ++comma++
                "n("++show  tid        ++comma++
                show ats ++comma++
                show ets     ++"))" ++ r


instance Show TypeId where
        showsPrec d (TypeId t) r = show t ++ r



comma      = "," 


