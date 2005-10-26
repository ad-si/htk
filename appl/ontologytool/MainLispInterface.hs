module Main (main, command_loop) where

{-
  todo:
    save layout (consistent with graph) (i.e. just layout positions?)
    hide links and nodes
    retype links and nodes
    orientation (up, down)
-}


import Char
import IO
import DaVinciGraph
import GraphDisp
import GraphConfigure
import AbstractGraphView
import Data.IORef


-- Auxiliary stuff for graph display commands

-- lexer and parser


-- lexer from Prelude, just adpated to recognize a-b as a single token
ourlex                    :: ReadS String
ourlex ""                  = [("","")]
ourlex (c:s) | isSpace c   = ourlex (dropWhile isSpace s)
ourlex ('\'':s)            = [('\'':ch++"'", t) | (ch,'\'':t)  <- lexLitChar s,
					       ch /= "'"                ]
ourlex ('"':s)             = [('"':str, t)      | (str,t) <- ourlexString s]
			  where
			  ourlexString ('"':s) = [("\"",s)]
			  ourlexString s = [(ch++str, u)
						| (ch,t)  <- ourlexStrItem s,
						  (str,u) <- ourlexString t  ]

			  ourlexStrItem ('\\':'&':s) = [("\\&",s)]
			  ourlexStrItem ('\\':c:s) | isSpace c
			      = [("",t) | '\\':t <- [dropWhile isSpace s]]
			  ourlexStrItem s            = lexLitChar s

ourlex (c:s) | isSingle c  = [([c],s)]
             | isSymAlphaNum c  = [(c:tok,t) | (tok,t) <- [span isSymAlphaNum s]]
   	     | otherwise   = []    -- bad character
		where
                isSingle c = c  `elem` "()"
		isSymAlphaNum c  =  
                    c `elem` ",;[]{}_`!@#$%&*+./<=>?\\^|:-~_'"
                 || isAlphaNum c 

lexmatch                   :: (Eq a) => [a] -> [a] -> ([a],[a])
lexmatch (x:xs) (y:ys) | x == y  =  lexmatch xs ys
lexmatch xs     ys               =  (xs,ys)

asciiTab = zip ['\NUL'..' ']
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
	    "SP"]


lexer s = case ourlex s of 
   [("","")] -> []
   [(s,rest)] -> s:lexer rest
   otherwise -> []

delQuotes ('\"':rest) = 
  case reverse rest of
    '\"':rest' -> reverse rest'
    otherwise -> '\"':rest
delQuotes s = s

data ParseResult = AP String
                 | LP [ParseResult]
     deriving (Show, Eq)

parse [] = (AP "",[])
parse (")":rest) = (AP "",rest)
parse ("(":rest) = (LP (reverse pl),rest')
  where
    (pl,rest') =  parseList1 [] rest
parse (x:rest) = 
  if map toLower x == "nil" 
  then (LP [],rest)
  else (AP (delQuotes x),rest)

parseList s = reverse (fst (parseList1 [] s))

parseList1 l [] = (l,[])
parseList1 l (")":rest) = (l,rest)
parseList1 l s = parseList1 (res1:l) rest
  where
  (res1,rest) = parse s 


-- try to read

try_to_read :: Read a => String -> Maybe a
try_to_read x = case reads x of
   [(y,"")] -> Just y
   otherwise -> Nothing



-- answer to commands

cmd_fails cid msg = do
  putStrLn ("(ANSWER "++cid++" NIL \""++msg++"\" )")

cmd_succeeds cid res = do
  putStrLn ("(ANSWER "++cid++" T "++res++")")

send_request gid name =
  putStrLn ("(REQUEST "++show gid++" "++name++")")

-- The graph display commands


local_action_node (gid::Int) action = (\(_,nid,_) -> send_request gid (show nid++" "++action))
local_action_edge (gid::Int) action = (\(_,eid) -> send_request gid (show eid++" "++action))
global_action (gid::Int) (action::String) = send_request gid action

make_menu act (LP [AP "BUTTON", AP name, AP action]) =
  Button name (act action)
make_menu act (AP "BLANK") =
  Blank
make_menu act (LP [AP "MENU",AP name,LP submenus]) =
  Menu (Just name) (map (make_menu act) submenus)
make_menu _ _ =
  Button "Error" undefined

get_node_shape "box" = Box
get_node_shape "circle" = Circle
get_node_shape "ellipse" = Ellipse
get_node_shape "rhombus" = Rhombus
get_node_shape "triangle" = Triangle
get_node_shape "icon" = Icon ""
get_node_shape _ = Box

make_nodeType gid (LP [AP "NODETYPE",AP name,AP shape,AP color,menu]) =
  (name,
   get_node_shape (map toLower shape) $$$
   Color color $$$
   ValueTitle (\(s,_,_) -> return s) $$$
   LocalMenu (make_menu (local_action_node gid) menu) $$$
   emptyNodeTypeParms :: DaVinciNodeTypeParms (String,Int,Int))
--make_nodeType graph _ _ _ = error

get_edge_shape "solid" = Solid
get_edge_shape "dotted" = Dotted
get_edge_shape "dashed" = Dashed
get_edge_shape "thick" = Thick
get_edge_shape "double" = Double
get_edge_shape _ = Solid

make_edgeType gid (LP [AP "EDGETYPE",AP name,AP shape,AP color,menu]) = 
   (name,
    get_edge_shape (map toLower shape) $$$
    Color color $$$
    LocalMenu (make_menu (local_action_edge gid) menu) $$$
    emptyArcTypeParms :: DaVinciArcTypeParms (String,Int))
--make_edgeType graph _ _ _ = error

make_edgeComp x = 
  case (sequence (map make_edgeComp1 x)) of
    Just l -> l
    Nothing -> []
  where
  make_edgeComp1 (LP [AP x, AP y, AP z]) = Just (x,y,z)
  make_edgeComp1 _ = Nothing

makegraph1 cid [AP title,LP menus,LP nodetypes,LP edgetypes,LP comptable] gv = do
  (gs,ev_cnt) <- readIORef gv
  Result gid err <- AbstractGraphView.makegraph title  -- ... graphs'...
                     (map (GlobalMenu . (make_menu (global_action ev_cnt))) menus)
                     (map (make_nodeType ev_cnt) nodetypes)
                     (map (make_edgeType ev_cnt) edgetypes)
                     (make_edgeComp comptable)
                     gv
  case err of
    Nothing -> cmd_succeeds cid (show gid) 
    Just e -> cmd_fails cid e
    
makegraph1 cid _ graphs = do
  cmd_fails cid "makegraph: illegal argument format"

makegraph cid args graph =
  catch (makegraph1 cid args graph)
        (\e -> cmd_fails cid "makegraph: illegal argument format") -- graph)

delgraph gid _ graphs =
    AbstractGraphView.delgraph gid graphs


addnode gid [AP nodetype, AP name] graphs = 
  AbstractGraphView.addnode gid nodetype name graphs
addnode _ _ graphs = do
  return_fail graphs "addnode: illegal argument format"


delnode gid [AP node] graphs =
  case try_to_read node :: Maybe Int of
    Just n' -> AbstractGraphView.delnode gid n' graphs 
    Nothing ->  return_fail graphs ("delnode: illegal node: "++node)
delnode _ _ graphs = do
  return_fail graphs "delnode: illegal argument format"


addlink gid [AP edgetype, AP name, AP src, AP tar] graphs = 
  case (try_to_read src,
        try_to_read tar) of
    (Just src_node, Just tar_node) -> 
       AbstractGraphView.addlink gid edgetype name src_node tar_node graphs
    (Nothing,_) -> return_fail graphs ("addlink: illegal source node id: "++src)
    (_,Nothing) -> return_fail graphs ("addlink: illegal target node id: "++tar)
addlink _ _ graphs = do
  return_fail graphs "addlink: illegal argument format"


dellink gid [AP edge] graphs =
  case try_to_read edge :: Maybe Int of
    Just e ->
       AbstractGraphView.dellink gid e graphs
    Nothing -> return_fail graphs ("dellink: illegal edge: "++edge)
dellink _ _ graphs = do
  return_fail graphs "dellink: illegal argument format"


redisplay gid _ graphs = 
  AbstractGraphView.redisplay gid graphs

unAPInt (AP x) = try_to_read x :: Maybe Int
unAPInt _ = Nothing

hidenodes gid [LP nodes] graphs =  
  case sequence (map unAPInt nodes) of
    Just nodes' -> AbstractGraphView.hidenodes gid nodes' graphs
    Nothing -> return_fail graphs "hidenodes: illegal argument format"
hidenodes _ _ graphs = do
  return_fail graphs "hidenodes: illegal argument format"

abstractnodes gid [LP nodes] graphs =  
  case sequence (map unAPInt nodes) of
    Just nodes' -> AbstractGraphView.abstractnodes gid nodes' graphs
    Nothing -> return_fail graphs "abstractnodes: illegal argument format"
abstractnodes _ _ graphs = do
  return_fail graphs "abstractnodes: illegal argument format"

hideedges gid [LP edges] graphs =  
  case sequence (map unAPInt edges) of
    Just edges' -> AbstractGraphView.hideedges gid edges' graphs
    Nothing -> return_fail graphs "hideedges: illegal argument format"
hideedges _ _ graphs = do
  return_fail graphs "hideedges: illegal argument format"

hideedgetype gid [AP edgetype] graphs =
  AbstractGraphView.hideedgetype gid edgetype graphs
hideedgetype _ _ graphs =
  return_fail graphs "hideedgetype: illegal argument format"

hidenodetype gid [AP nodetype] graphs =
  AbstractGraphView.hidenodetype gid nodetype graphs
hidenodetype _ _ graphs =
  return_fail graphs "hidenodetype: illegal argument format"

showIt gid [AP ev_id] graphs = 
  case try_to_read ev_id :: Maybe Int of
    Just ev_id' -> AbstractGraphView.showIt gid ev_id' graphs 
    Nothing ->  return_fail graphs ("showIt: illegal event id: "++ev_id)
showIt _ _ graphs = do
  return_fail graphs "showIt: illegal argument format"


command "makegraph" cid args gv = Main.makegraph cid (parseList args) gv
--command "delgraph" cid (gid:_) graphs = delgraph cid gid graphs
command "quit" cid _ gv = do
  delallgraphs gv
  cmd_succeeds cid ""


command c cid [] gv = 
   cmd_fails cid (c++" needs a graph id")
command c cid (gid:args) gv = do
  case (try_to_read gid,
        lookup c
        [("delgraph",Main.delgraph),
         ("addnode",Main.addnode), 
         ("delnode",Main.delnode), 
         ("addlink",Main.addlink), 
         ("dellink",Main.dellink), 
         ("redisplay",Main.redisplay),
         ("hidenodes",Main.hidenodes),
         ("hidenodetype",Main.hidenodetype),
         ("abstractnodes",Main.abstractnodes),
         ("show",Main.showIt),
         ("hideedges",Main.hideedges),
	 ("hideedgetype",Main.hideedgetype)]) of
    (_,Nothing) -> cmd_fails cid ("unknown command: "++c)
    (Nothing,_) -> cmd_fails cid ("illegal graph id: "++gid)
    (Just gid', Just cmd) -> do 
       Result descr err <- cmd gid' (parseList args) gv
       case err of
         Nothing -> cmd_succeeds cid (show descr)
         Just e -> cmd_fails cid e


command_loop gv =
  do s <- getLine
     let lexres = lexer s
     case lexres of
        "(":c:cid:args -> command (map toLower c) cid args gv
	otherwise -> return ()
     case lexres of
        "(":"quit":_ -> return ()
        otherwise -> command_loop gv
                           
 
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "(ANSWER T \"This is the abstraction viewer for daVinci\")"
  gv <- initgraphs
--  s <- readFile "testAbstractnodes.data"
--  sequence (map (process_command gv) (lines s))
  command_loop gv
  where
  process_command gv s =
    do let lexres = lexer s
       case lexres of
          "(":c:cid:args -> command (map toLower c) cid args gv
  	  otherwise -> return ()


-- LispActions send to DaVinci:
-- ============================

-- (MAKEGRAPH ActionId Knotentypen Kantentypen KompositionsTabelle)
--   VALUE: (ANSWER ActionId GRAPHID) (Integer)

-- Knotentypen = ()
--             | (KnotenTyp . KnotenTypen)
-- KnotenTyp   = (NODETYPE NameString Shape Menu)
-- Shape       = "Box" | "Circle" | "Ellipse" | "Rhombus" | "Triangle" | "Icon"

-- Menu        = (BUTTON PrintString ActionName) (* Send (REQUEST (ActionName  Edge-or-NodeId)) *)
--             | (MENU PrintString MenuList)
--             | BLANK

-- MenuList = ()
--          | (Menu . MenuList)

-- KantenTypen = () | (KantenTyp . KantenTypen)
-- KantenTyp   = (EDGETYPE NameString EdgeShape Color Menu)

-- EdgeShape   = "Solid" | "Dotted" | "Dashed" | "Thick" | "Double"

-- Color       = string

-- KompositionsTabelle = ()
--                     | (KompositionInfo . KompositionsTabelle)

-- KompositionInfo = (EdgeTypeNameString EdgeTypeNameString EdgeTypeNameString)

-- (DELGRAPH ActionId GraphId)
--   VALUE: (ANSWER ActionId Boolean)

-- (ADDNODE ActionId GraphId NodeTypeNameString PrintName)
--   VALUE: (ANSWER ActionId NodeId) (Integer)

-- (DELNODE ActionId GraphId NodeId)
--   VALUE: (ANSWER ActionId Boolean)

-- Boolean = T | NIL

-- (ADDLINK ActionId GraphId EdgeTypeNameString PrintName SourceNodeId TargetNodeId)
--   VALUE: (ANSWER ActionId EdgeId) (Integer)

-- (DELLINK ActionId GraphId EdgeId)
--   VALUE: (ANSWER ActionId Boolean)

-- (REDISPLAY ActionId GraphId)
--   VALUE: (ANSWER ActionId Boolean)

-- (HIDENODES ActionId GraphId NodeIdList)
--   VALUE: (ANSWER ActionId EventId) (Integer)

-- NodeIdList = () | (NodeId . NodeIdList)

-- (ABSTRACTNODES ActionId GraphId NodeIdList)
--   VALUE: (ANSWER ActionId EventId) (Integer)

-- (SHOWNODES ActionId GraphId EventId)
--   VALUE: (ANSWER ActionId Boolean)

-- (HIDEEDGES ActionId GraphId EdgeTypeNameString)
--   VALUE: (ANSWER ActionId Boolean)

-- (SHOWEDGES ActionId GraphId EdgeTypeNameString)
--   VALUE: (ANSWER ActionId Boolean)


-- Input From DaVinci:
-- ===================

-- (ANSWER ActionId T args)  : the answer to the action send to davinci of id ActionId
-- (REQUEST DaVinciAction) : a request from davinci invoked by user
-- (ANSWER ActionId NIL errmsg)





