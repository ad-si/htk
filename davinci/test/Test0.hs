{- This test case is obsolete!! -}
module Main (
        main 
        ) 
where

import Concurrency
import HTk
import DialogWin
import IconBar
import PulldownMenu
import Button
import MenuButton
import Separator
import Colour
import Bell
import DaVinci
import Debug(debug)


title = text

main :: IO ()
main = 
   do
      gui <- htk [ {- logfile (1::Int), -} text "DAVINCI-TEST"]
      gtool <- davinci [accuracy 1]
      interactor (const (destroyed gtool >>> destroy gui))
      setDefaults                                             
      dragAndDrop                                            
      createGraphM1                                          
      createGraph0                                            
      (g0,r0) <- createGraph1                          
      createGraph2                                            
      (g,r2,mv)<- createGraph3                                         
      next "install icon bar" (makeIconBar >>= \ ib -> iconbar ib g)
      try(next "install application menu" 
         (do
            mn <- makeMenu
            configure g [applicationmenu mn]
            done
            ))                                     
      next "hide subgraph" (hideSubgraph [r2])                
      next "show subgraph" (showSubgraph [r2])                
      next "select children" (selectChildren r2)              
      next "color red-green" (colorRedGreen r2 >> redrawGraph g)      
      next "color red-yellow-green" (colorRedBlueGreen g0 r0) 
      next "delete tree" (destroy r2 >> redrawGraph g )       
      next "iconify" (iconify mv)                          
      next "deiconify" (deiconify mv)                      
      next "close graph" (destroy g)                       
      next "end session" (done)                            
      destroy gui                                           
      done
   where 
      next :: String -> IO a -> IO ()
      next str cmd =
         do
            newAlertWin ("press button to " ++ str ++ "\n") [] 
            putStr ("starting command" ++ str ++ "\n")      
            try cmd                                           
            putStr ("finished command" ++ str ++ "\n")      
            done


setDefaults :: IO ()
setDefaults = 
  do
     mn <- makeMenu
     g <- newGraph [fontsize 14, title "Testing Default"]
     nt <- getDefaultNodeType g
     configure nt [
             bg "green", 
             border DoubleBorder, 
             shape Circle, 
             applicationmenu mn
             ]
     et <- getDefaultEdgeType g
     configure et [
             bg "pink", 
             pattern ThickLine,
             applicationmenu mn
             ]
     rulesTakePrecedence True g
     
     r <- newNode g Nothing [text "root"]
     createTree r 1 4
     displayGraph g
     done                            
     

dragAndDrop :: IO ()
dragAndDrop = 
   do
      mn <- makeMenu
      g <- newGraph [fontsize 14, title "Drag N Drop",dragging On]
      nt <- getDefaultNodeType g
      configure nt [
         bg "green", 
         border DoubleBorder, 
         shape Circle, 
         applicationmenu mn
         ]
      et <- getDefaultEdgeType g
      configure et [
         bg "pink", 
         pattern ThickLine,
         applicationmenu mn
         ]
      
      r <- newNode g Nothing [text "root"]
      createTree r 1 4
      displayGraph g
      interactor (\iact -> 
            destroyed g >>> stop iact
         +> nodeSelected g >>> done
         +> edgeSelected g >>> done
         +> nodeDoubleClicked g >>> done
         +> edgeDoubleClicked g >>> done
         +> popupSelectionNode g >>> done
         +> popupSelectionEdge g >>> done
         +> createNodeGesture g >>> 
               do
                  newNode g Nothing [bg "red"]
                  redrawGraph g 
                  done
         +> createChildGesture g >>>= (\src ->  
               do
                  trg <- newNode g Nothing [bg "blue"] 
                  redrawGraph g
                  newEdge Nothing src trg [bg "blue"]
                  redrawGraph g 
                  done
               )
         +> createEdgeGesture g >>>= (\(src,trg) -> 
               do
                  newEdge Nothing src trg [bg "red"] 
                  redrawGraph g 
                  done
               )
         )
      done                            
      

createGraphM1 :: IO ()
createGraphM1 = do {
        g <- newGraph [fontsize 14, compact On, title "Moby Design"];
        n <- newNode g (Just "n1") [text "Moby Design Base"];
        n2 <- getNode g (NodeId "n1");
        b <- existNode n2;
        unless b deadlock; 
        displayGraph g;
        newSurveyView g;
        done
        }


createGraph0 :: IO ()
createGraph0 = 
        newGraph [fontsize 14, compact On, title "Moby Design"]         >>= \g ->
        newNode g Nothing [text "Moby Design Base"]             >>= \ r ->
        ancestor r [text "Design 1", shape Box]                 >>= \ d1 ->
        ancestor r [text "Design 2", shape Box]                 >>= \ d2 ->
        ancestor r [text "Design 3", shape Box]                 >>= \ d3 ->
        ancestor d1 [text "Z Spec 1", shape Rhombus]            >>= \ z1 ->
        ancestor d1 [text "Z Spec 1", shape Rhombus]            >>= \ z2 ->
        displayGraph g                                          >>
        newSurveyView g                                 >>
        done


createGraph1 :: IO (Graph,Node)
createGraph1 = 
        putStr ("CREATING GRAP 1\n")                            >>
        newGraph [fontsize 14, compact On, title "Binary Tree"]         >>= \g ->
        newNode g Nothing [text "root"]                         >>= \ r ->
        createTree r 1 4                                        >>
        configure r [fontfamily Helvetica, shape Rhombus]               >>
        displayGraph g                                          >>
        newSurveyView g                                 >>
        return (g,r)


createGraph2 =
        putStr ("CREATING GRAP 2\n")                            >>
        newGraph [fontsize 14, compact On, gapwidth 100, 
               gapheight 100, graphorientation RightToLeft]     >>= \ g1 ->
        configure g1 [filename "graph_example.daVinci"]         >> 
        return g1


createGraph3 =
        putStr ("CREATING GRAP 3\n")                            >>
        newGraph []                                             >>= \ g ->
        newNode g (Just "#11") [text "root I", fg red, 
                shape Ellipse, border SingleBorder]             >>= \n1 ->      
        newNode g (Just "#12")[text "parent", fg red, 
           shape Ellipse, border SingleBorder,
           child Nothing [bmap' "juggler1.bm"],
           child Nothing [bmap' "kangro1.bm"],
           child Nothing [bmap' "king.bm"]
           ]                                                    >>= \n2 ->
        newNode g Nothing [text "root II", fg green, 
                fontfamily Helvetica,
                shape Rhombus, border DoubleBorder]             >>= \ r2 ->
        createTree r2 0 3                                       >>
        newEdge (Just "!1") n1 n2 [fg red]                      >>= \ e1 ->
        configure e1 [pattern DashedLine]                       >>
        displayGraph g                                          >>= \mv ->
        interactor (\iact -> 
                nodeSelected g >>>= (\nds -> foreach nds (fg pink) >> redrawGraph g) 
            +>  edgeSelected g >>>= (\e -> fg pink e >> redrawGraph g)
            +>  (destroyed g >>> (closeGraph g >> stop iact))
            )                                                   >>
        newSurveyView g                                         >>
        newDetailedView g                                       >>      
        configure g [selection [n2]]                            >>      
        configure g [title "title", showmsg "text",statusmsg "status"]  >>
        return (g,r2,mv)


createTree p x y | x > y = done
createTree p l y  =
                ancestor p [text (show l), shape Box]           >>= \ c1 ->
                ancestor p [text (show l), shape Circle]        >>= \ c2 ->
                createTree c1 (l +1) y                          >>
                createTree c2 (l + 1) y



makeIconBar :: IO (IconBar String)
makeIconBar = do {
        ib <- newIconBar [];
        newButton [reaction (return "i1"),bmap' "trash.bm",parent ib];
        newButton [reaction (return "i2"),bmap' "bullseye.bm",parent ib];
        newButton [reaction (return "i3"),bmap' "ms_down_arrow.bm",parent ib];
        newButton [reaction (return "i4"),bmap' "ms_up_arrow.bm",parent ib];
        newButton [reaction (return "i5"),bmap' "ms_left_arrow.bm",parent ib];
        newButton [reaction (return "i6"),bmap' "ms_right_arrow.bm",parent ib];
        controller ib (const (receive ib >>> bell));
        return ib
}


bmap' :: HasBitMap w => String -> Config w
bmap' fnm = bitmap ("/home/ewk/programs/workbench/htk/test/icons/" ++ fnm)


makeMenu :: IO (Menu ())
makeMenu =
        newEventLoop                                            >>= \el ->
        newMenu []                                                      >>= \ mn ->
        button [text "Start", action el (\w -> receive w >>> ring), parent mn]  >>
        button [text "Query", action el (\w -> receive w >>> ring), parent mn]  >>
        newSeparator [parent (mn::Menu ())]                                             >>

        newCascadeMenu [text "Repository", parent mn]           >>= \sm ->
        button [text "Load", action el (\w -> receive w >>> ring), parent sm]   >>
        button [text "Save", 
                action el (\w -> receive w >>> ring), 
                parent (sm::Menu ())
                ]                                               >>
        
        newSeparator [parent mn]                                                >>
        button [text "Quit", action el (\w -> receive w >>> ring), parent mn]   >>
        enterEventLoop el                                               >>
        return mn

ring = bell

colorRedBlueGreen :: Graph -> Node -> IO ()
colorRedBlueGreen g n = 
        forkIO (colorTree g n red blue) >>
        forkIO (colorTree g n blue green) >>
        done

colorRedGreen :: Node -> IO ()
colorRedGreen n = 
        configure n [fg red]                                    >>
        getOutgoing n                                           >>= \ edg ->
        foreach edg (\ e -> configure e [fg green])             >>
        getChildren n                                           >>= \ chs ->
        foreach chs colorRedGreen


colorTree g n nc ec =
        configure n [fg nc]                                     >>
        getOutgoing n                                           >>= \ edg ->
        foreach edg (\ e -> configure e [fg ec])                >>
        redrawGraph g                                           >>
        getChildren n                                           >>= \ chs ->
        foreach chs (\n' -> colorTree g n' nc ec)


red = toColour "red"
pink = toColour "pink"
green = toColour "green"
blue = toColour "blue"
