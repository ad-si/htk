{- GetAttributes is used by the GraphEditor to pop up HTk windows
   to get information from the user.
   -}
module GetAttributes(
   SingleShape(..),
   NodeTypeAttributes(..), 
   getNodeTypeAttributes, -- :: IO NodeTypeAttributes
   ) where

import HTk
import DialogWin
import InputWin

------------------------------------------------------------------------
-- NodeTypeAttributes
------------------------------------------------------------------------

data SingleShape = BOX | CIRCLE | ELLIPSE | RHOMBUS | TRIANGLE | ICON
   deriving (Enum,Read,Show)

instance GUIValue SingleShape where
   cdefault = BOX

data NodeTypeAttributes = NodeTypeAttributes {
   nodeTypeShape :: SingleShape,
   iconLocation :: String
   } deriving Show

getNodeTypeAttributes =
   do
      let
         def = NodeTypeAttributes {
            nodeTypeShape = BOX,
            iconLocation = ""
            }
      inputWin <- newNodeTypeInputWin def
      nodeTypeAttributesOpt <- sync (triggered inputWin)
      case nodeTypeAttributesOpt of
         Nothing -> return def
         Just nodeTypeAttributes -> return nodeTypeAttributes

newNodeTypeInputWin :: NodeTypeAttributes -> 
   IO (InputWin (Maybe NodeTypeAttributes))
newNodeTypeInputWin nodeTypeAttributes =
   do
      form <- newInputForm [flexible,value nodeTypeAttributes]
      newEnumField [BOX .. ICON] [
         text "Shape",
         selector nodeTypeShape,
         modifier (\ old newShape -> old {nodeTypeShape = newShape}),
         parent form
         ]
      newTextField [
         text "Icon File Location",
         selector (lines . iconLocation),
         modifier (\ old newText -> old {iconLocation = listToName newText}),
         parent form,
         width 20,
         height 1
         ]
      newInputWin "Node Attributes" form Nothing [modal True]

listToName :: [String] -> String
listToName [name] = name
listToName _ = error "Too many names"



