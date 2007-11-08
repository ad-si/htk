{------------------------------------
MODULE        : UDraw.Communication
AUTHOR        : Simon Drees,
                University of Bremen
DATE          : 2006
VERSION       : 1.0
DESCRIPTION   : Provides communication-strings, to work with uDraw
-------------------------------------}

module UDraw.Communication where

import qualified MMiSS.MMiSSOntology        as MOnto
import qualified Debug.Trace                as Debug
import qualified UDraw.OntoSideConfig       as OntoConf

--------------------------------------------
-- sent init strings
--
-- it's a bit tricky, because these are more
--------------------------------------------
initOntoToolStrings :: [String]
initOntoToolStrings =
  ["special(add_language(\"resource/ontotool\"))",
  "gui(configure(\"resource/ontotool\"))",
  "window(title(\"New Graph\"))",
  "window(show_status(\"uDraw (Ontology) with Sidebar\"))",
  "visual(new_rules(["++
    "nr(\"\",[m(["++
      "menu_entry(\"shPar\",\"Show parents\"),"++
      "menu_entry(\"shSib\",\"Show siblings\"),"++
      "menu_entry(\"inRel\",\"In relation\")])]),"++
    "er(\"\",[m(["++
      "menu_entry(\"hideRel\",\"Hide this relation\")])])]))",
  "drag_and_drop(dragging_on)",
  "gui(activate_menus([\"#%open\",\"#%exit\",\"S:H\"]))",
  "gui(activate_icons([\"#%open\"]))",
  "window(show_message(\"\"))"]


--   "nr(\"\",[m([submenu_entry(\"Abstraction\",\"#%Abstraction\",[menu_entry(\"Abstraction.HideSub\",\"#%Abstraction.HideSub\"),menu_entry(\"Abstraction.ShowSub\",\"#%Abstraction.ShowSub\"),menu_entry(\"Abstraction.HideEdge\",\"#%Abstraction.HideEdge\"),menu_entry(\"Abstraction.ShowEdge\",\"#%Abstraction.ShowEdge\")]),blank,submenu_entry(\"Navigation\",\"#%Navigation\",[menu_entry(\"Navigation.Parents\",\"#%Navigation.Parents\"),menu_entry(\"Navigation.Siblings\",\"#%Navigation.Siblings\"),menu_entry(\"Navigation.Children\",\"#%Navigation.Children\")])])]),er(\"\",[])]))",
--------------------------------------------
-- sent informations to start file-dialog
--------------------------------------------
openFileDialog :: String -> String
openFileDialog pwd=
  let messages = ["window(file_browser(true,\"#%FileDlg.OntoTool.Title.Open\",\"#%FileDlg.OK.Open\",\"" ++
                  pwd ++ "\",\"\",[bt(\"#%FileDlg.All\",\"*.*\",\"\")," ++
                  "bt(\"#%FileDlg.GraphType\",\"*.udg\",\"#%FileDlg.Graph\")," ++
                  "bt(\"#%FileDlg.StatusType\",\"*.status\",\"#%FileDlg.Status\")],false))"]
  in debugMessage messages ""
  where
  debugMessage :: [String] -> String -> String
  debugMessage []         msgs = msgs
  debugMessage (msg:rest) msgs = debugMessage rest ((Debug.trace ("OntoSide: " ++ msg) msgs) ++ msg ++ "\n")

--------------------------------------------
-- sent exit-string
--------------------------------------------
exit :: String
exit = Debug.trace "OntoSide: menu(file(exit))" "menu(file(exit))\n"

--------------------------------------------
-- sent improve-layout-string
--------------------------------------------
improveLayout :: String
improveLayout = Debug.trace "OntoSide: menu(layout(improve_all))" "menu(layout(improve_all))\n"

--------------------------------------------
-- sent HideSubgraph-String
--------------------------------------------
hideSubgraph :: String -> String
hideSubgraph nId =
  Debug.trace ("OntoSide: menu(abstraction(hide_subgraph([" ++
  nId ++ "])))") ("menu(abstraction(hide_subgraph([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent HideSubgraph-String
--------------------------------------------
showSubgraph :: String -> String
showSubgraph nId =
  Debug.trace ("OntoSide: menu(abstraction(show_subgraph([" ++
  nId ++ "])))") ("menu(abstraction(show_subgraph([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent HideEdge-String
--------------------------------------------
hideEdge :: String -> String
hideEdge nId =
  Debug.trace ("OntoSide: menu(abstraction(hide_edges([" ++
  nId ++ "])))") ("menu(abstraction(hide_edges([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent showEdge-String
--------------------------------------------
showEdge :: String -> String
showEdge nId =
  Debug.trace ("OntoSide: menu(abstraction(show_edges([" ++
  nId ++ "])))") ("menu(abstraction(show_edges([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent showParents-String
--------------------------------------------
showParents :: String -> String
showParents nId =
  Debug.trace ("OntoSide: menu(navigation(select_parents([" ++
  nId ++ "])))") ("menu(navigation(select_parents([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent showSiblings-String
--------------------------------------------
showSiblings :: String -> String
showSiblings nId =
  Debug.trace ("OntoSide: menu(navigation(select_siblings(["++
  nId ++ "])))") ("menu(navigation(select_siblings([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent showChildren-String
--------------------------------------------
showChildren :: String -> String
showChildren nId =
  Debug.trace ("OntoSide: menu(navigation(select_children([" ++
  nId ++ "])))") ("menu(navigation(select_children([" ++ nId ++ "])))\n")

--------------------------------------------
-- sent zoom-string
--------------------------------------------
zoom :: Int -> String
zoom x = " menu(view(scale(" ++ (show x) ++")))\n"

--------------------------------------------
-- get a node, represented by its id, name,
-- type and a list of edges.
-- The first parameter indicates if this
-- function is called from an update or on
-- creation of a graph.
--------------------------------------------
createNode :: Bool -> Int -> String -> String -> [(String, Int, Int)] -> MOnto.OntoObjectType -> OntoConf.Config -> String
createNode update nId nName _ edges nType config =
  let OntoConf.Config nodes _ _ = config
  in if update
        then
          "new_node(\"" ++ (show nId) ++ "\",\"Class\",["
          ++ (if nType == MOnto.OntoClass then createNodeAttrib nName (getClass nodes) else createNodeAttrib nName (getObject nodes)) ++ "])"
        else
          "l(\"" ++ (show nId) ++ "\",n(\"Class\",["
          ++ (if nType == MOnto.OntoClass then createNodeAttrib nName (getClass nodes) else createNodeAttrib nName (getObject nodes))
          ++ "],[" ++ (createEdges False edges config) ++ "])),"
  where
      getClass :: OntoConf.Nodes -> OntoConf.Node
      getClass (OntoConf.Nodes nodes) =
        foldl1 isClass nodes
        where
        isClass x y
            | (OntoConf.nodeType x) == "class" = x
            | otherwise = y
      getObject :: OntoConf.Nodes -> OntoConf.Node
      getObject (OntoConf.Nodes nodes) =
        foldl1 isObject nodes
        where
        isObject x y
            | (OntoConf.nodeType x) == "object" = x
            | otherwise = y

--------------------------------------------
-- create node-delelte-string
--------------------------------------------
deleteNode :: Int -> String
deleteNode nId = "delete_node(\"" ++ (show nId) ++ "\")"

--------------------------------------------
-- create edge-delelte-string
--------------------------------------------
deleteEdge :: String -> Int -> Int -> String
deleteEdge eType startId targetId = "delete_edge(\"" ++ (show startId) ++ eType ++ (show targetId) ++ "\")"

--------------------------------------------
-- get a node, represented by its id, name,
-- type and a list of edges.
--------------------------------------------
updateNodeAttributes :: Int -> String -> String -> [(String, Int, Int)] -> MOnto.OntoObjectType -> OntoConf.Config -> String
updateNodeAttributes nId nName _ edges nType config =
  let OntoConf.Config nodes _ _ = config
  in "node(\"" ++ (show nId) ++ "\",["
      ++ (if nType == MOnto.OntoClass
            then createNodeAttrib nName (getObject "class" nodes)
            else createNodeAttrib nName (getObject "object" nodes))
      ++ "])," ++
      (updateEdgesAttributes edges config)
  where
      getObject :: String -> OntoConf.Nodes -> OntoConf.Node
      getObject searchString (OntoConf.Nodes nodes) =
        foldl1 isObject nodes
        where
        isObject x y
            | (OntoConf.nodeType x) == searchString = x
            | otherwise = y

--------------------------------------------
-- get all edges, represented by its type and
-- its target id
--------------------------------------------
createEdges :: Bool -> [(String, Int, Int)] -> OntoConf.Config -> String
createEdges _ [] _ = ""
createEdges update (edge:rest) config =
  (createEdge update edge config) ++ createEdges update rest config

--------------------------------------------
-- get edge, represented by its type and
-- its target id
--------------------------------------------
createEdge :: Bool -> (String, Int, Int) -> OntoConf.Config -> String
createEdge update (eType, startId, targetId) config =
  let OntoConf.Config _ rels _ = config
  in if update
        then  "new_edge(\"" ++ (show startId) ++ eType ++ (show targetId) ++
              "\",\"" ++ eType++ "\",[" ++ (createEdgeAttrib eType (getEdge eType rels))  ++
              "],\"" ++ (show startId) ++ "\",\"" ++ (show targetId) ++
              "\")"
        else  "l(\"" ++ (show startId) ++ eType ++ (show targetId) ++
              "\",e(\"" ++ eType++ "\",[" ++ (createEdgeAttrib eType (getEdge eType rels))  ++
              "],r(\"" ++ (show targetId) ++ "\"))),"
  where
    getEdge :: String -> OntoConf.Relations -> OntoConf.Relation
    getEdge eName (OntoConf.Relations rels) =
      foldl1 isClass rels
      where
      isClass x y
          | (OntoConf.relationLabel x) == eName = x
          | otherwise = y

--------------------------------------------
-- get all edges, represented by its type and
-- its target id
--------------------------------------------
updateEdgesAttributes :: [(String, Int, Int)] -> OntoConf.Config -> String
updateEdgesAttributes [] _ = ""
updateEdgesAttributes ((eType, startId, targetId):rest) config =
  let OntoConf.Config _ rels _ = config
  in "edge(\"" ++ (show startId) ++ eType ++ (show targetId) ++"\",[" ++
  (createEdgeAttrib eType (getEdge eType rels))  ++ "])," ++ updateEdgesAttributes rest config
  where
    getEdge :: String -> OntoConf.Relations -> OntoConf.Relation
    getEdge eName (OntoConf.Relations rels) =
      foldl1 isClass rels
      where
      isClass x y
          | (OntoConf.relationLabel x) == eName = x
          | otherwise = y

--------------------------------------------
-- create attributes for a node
--------------------------------------------
createNodeAttrib :: String -> OntoConf.Node -> String
createNodeAttrib nName cla =
  "a(\"OBJECT\",\"" ++ nName ++ "\")," ++
  "a(\"FONTSTYLE\",\"" ++ (OntoConf.nodeFontStyle cla) ++ "\")," ++
  "a(\"FONTFAMILY\",\"" ++ (OntoConf.nodeFontFamily cla) ++ "\")," ++
  "a(\"BORDER\",\"" ++ (OntoConf.nodeBorder cla) ++ "\")," ++
  "a(\"_GO\",\"" ++ (OntoConf.nodeShape cla) ++ "\")"

--------------------------------------------
-- create attributes for an edge
--------------------------------------------
createEdgeAttrib :: String -> OntoConf.Relation -> String
createEdgeAttrib eName rel =
  "a(\"OBJECT\",\"" ++ (if (OntoConf.relationShowLabel rel) == "true" then (OntoConf.relationLabel rel) else "") ++ "\")," ++
  "a(\"FONTSTYLE\",\"" ++ (OntoConf.relationFontStyle rel) ++ "\")," ++
  "a(\"FONTFAMILY\",\"" ++ (OntoConf.relationFontFamily rel) ++ "\")," ++
  "a(\"EDGEPATTERN\",\"" ++ (OntoConf.relationEdgePattern rel) ++ "\")," ++
  "a(\"HEAD\",\"" ++ (OntoConf.relationHead rel) ++ "\")"
