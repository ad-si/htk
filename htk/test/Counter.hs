{- #########################################################################

MODULE        : Counter
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Composite Counter Widget


   ######################################################################### -}


module Counter (
        Counter,
        newCounter
        ) 
where

import HTk
import Prompt
import Button
import Debug(debug)


-- --------------------------------------------------------------------------
-- Definition 
-- --------------------------------------------------------------------------           
data Counter a = Counter Box (Prompt a) [Button (a -> a)]

-- --------------------------------------------------------------------------
-- Creation 
-- --------------------------------------------------------------------------           
newCounter :: [Config (Counter Int)] -> IO (Counter Int)
newCounter confs = do {
        vb <- newVFBox [];
        prt <- newPrompt [value (0::Int), text "Count",parent vb];
        bup <- newButton [text "Increment",cmd succ,parent vb];
        bdown <- newButton [text "Decrement",cmd pred, parent vb];
        binit <- newButton [text "Initialize",cmd (const 0),parent vb];
        configure (Counter vb prt [bup,bdown,binit]) confs
} where cmd :: (Int -> Int) -> Config (Button (Int -> Int))
        cmd f = command (\() -> return f)



-- --------------------------------------------------------------------------
-- Instantiations 
-- --------------------------------------------------------------------------           
instance Eq (Counter a) where 
        w1 == w2 = (toGUIObject w1) == (toGUIObject w2)

instance Ord (Counter a) where 
        w1 <= w2 = (toGUIObject w1) <= (toGUIObject w2)

instance GUIObject (Counter a) where 
        toGUIObject (Counter vbox _ _) = toGUIObject vbox
        cname _ = "Counter"

instance Destructible (Counter a) where
        destroy   = destroy . toGUIObject
        destroyed = destroyed . toGUIObject

instance Interactive (Counter a)

instance Widget (Counter a) where
        cursor c cnt @ (Counter bx pr bts) = synchronize cnt ( do {
                cursor c bx;
                cursor c pr;
                mapM (cursor c) bts;
                return cnt
                })

instance ChildWidget (Counter a) 

instance HasColour (Counter a) where
        legalColourID _ _ = True
        setColour cnt @ (Counter bx pr bts) cid c = synchronize cnt (do {
                setColour bx cid c;
                setColour pr cid c;
                mapM (\b -> setColour b cid c) bts; 
                return cnt
                }) 

instance HasFont (Counter a) where
        font f cnt @ (Counter _ pr bts) = synchronize cnt (do { 
                font f pr;
                mapM (font f) bts;
                return cnt
                })
        getFont (Counter bx pr _) = getFont pr

instance HasBorder (Counter a) 

instance HasSize (Counter a)

instance Synchronized (Counter a) where
        synchronize w = synchronize (toGUIObject w)


-- --------------------------------------------------------------------------
-- Commands 
-- --------------------------------------------------------------------------           
instance GUIValue a => Variable Counter a where
        setVar cnt@(Counter _ prt _) val  = 
                synchronize cnt (configure prt [value val] >> done)
        getVar cnt@(Counter _ prt _)   = synchronize cnt (getVar prt)
        updVar cnt@(Counter _ prt _) f = synchronize cnt (do {
                count <- getValue prt;
                (count',res) <- f count;
                value count' prt; 
                return res
                })              

-- --------------------------------------------------------------------------
-- Events 
-- --------------------------------------------------------------------------           
instance GUIValue a => Reactive Counter a where
        triggered (cnt @ (Counter _ _ bts)) = 
                choose (map triggered bts) >>>= updValue cnt

instance GUIValue a => HasTrigger Counter a where
        getTrigger = return . triggered
