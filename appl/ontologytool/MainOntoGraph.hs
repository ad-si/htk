module Main where

{--
OntoGraph ist ein Tool für die graphische Visualisierung von Ontologien.
Als Eingabeformat wird ein ASCII-File mit MMiSSLaTeX-Ontologie-Befehlen erwartet.
Es kann sich dabei um ein LaTeX- bzw. MMiSSLaTeX-File handeln - dies ist aber 
keine Voraussetzung.

--}

import AbstractGraphView
import DaVinciGraph
import GraphDisp
import GraphConfigure
import OntoParserOld

import Computation hiding (try)
import Parsec
import ParsecError
import List
import System

main =
  do args <- getArgs
     ok <- if (args == []) then ioError(userError "You must specify a file to process") else return(True)        
     filename <- return(last args)
     insertMissingClassesOption <- if ((elemIndices "-i" args) == []) then return(False) else return(True)
     useRelationTexts <- if ((elemIndices "-t" args) == []) then return(False) else return(True) 
     ontoEl <- parseMMiSSOntologyFile filename
     (ontology, warnings) <- case (fromWithError ontoEl) of
                   Right r -> return r
                   Left err -> ioError(userError err)
     ginfo <- initgraphs
     Result gid err <-
       makegraph "Ontologie"
           [GlobalMenu (Button "Knopf1" (putStrLn "Knopf1 wurde gedrückt")),
            GlobalMenu (Button "Knopf2" (putStrLn "Knopf2 wurde gedrückt"))]
           [("class", Box $$$ Color "azure2" $$$
                   LocalMenu (Menu (Just "Menu1") 
                    [Button "Knopf3" (\(s,_,_) -> putStrLn ("Knopf3 von Knoten "++s++" wurde gedrückt")),
                     Button "Knopf4" (\(s,_,_) -> putStrLn ("Knopf4 von Knoten "++s++" wurde gedrückt"))])
                   $$$ ValueTitle ( \ (name,descr,gid) -> return name) $$$
                   emptyNodeTypeParms :: DaVinciNodeTypeParms (String,Int,Int)
            ),
            ("object", Box  $$$ Color "#ffffA0" $$$
                   LocalMenu (Menu (Just "Menu1") 
                    [Button "Knopf5" (\(s,_,_) -> putStrLn ("Knopf5 von Knoten "++s++" wurde gedrückt")),
                     Button "Knopf6" (\(s,_,_) -> putStrLn ("Knopf6 von Knoten "++s++" wurde gedrückt"))])
                   $$$ ValueTitle ( \ (name,descr,gid) -> return name) $$$
                   emptyNodeTypeParms :: DaVinciNodeTypeParms (String,Int,Int)
            )]
           [("subClassOf", 
               Thick
               $$$ Dir "first"
               $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int)
            ),
            ("instanceOf", 
               Dotted
               $$$ Dir "none"
               $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int)
            ),
            ("relation",
               Solid
               $$$ Head "arrow"
               $$$ ValueTitle (\ (name, _) -> return name)
--               $$$ TitleFunc (\ (name, _) -> name)
               $$$ emptyArcTypeParms :: DaVinciArcTypeParms (String,Int)
            )]
           []
           ginfo
--     [Result nid1 _,Result nid2 _] <- sequence [addnode gid "class" "Package" ginfo,addnode gid "class" "StructuralEntity" ginfo]
     nodeList <- insertClasses (classes ontology) [] gid ginfo
     nodeList1 <- insertRelations insertMissingClassesOption useRelationTexts (relations ontology) nodeList gid ginfo
     objList <- insertObjects (objects ontology) nodeList [] gid ginfo
     objList1 <- insertLinks useRelationTexts (objectLinks ontology) (relations ontology) objList gid ginfo
     redisplay gid ginfo
--     Result eid err2 <- addlink gid "relation" "RelationTitle" nid1 nid2 ginfo
--     putStr (show ontology)
     getLine
--     delgraph gid ginfo
         

insertClasses :: [ClassDecl] -> [(String, Descr)] -> Descr -> GraphInfo -> IO ([(String, Descr)])
insertClasses (c:classes) nodeList gid ginfo = 
  do newNodeList <-  
        case (super c) of
          Just(superClass) -> 
             do (superNID, nodeList2) <- 
                  case (lookup superClass nodeList) of
                    Just(superID) -> return((superID, nodeList))
                    Nothing -> do Result nid _ <- addnode gid "class" superClass ginfo
                                  return((nid, nodeList ++ [(superClass, nid)]))
                (subNID, nodeList3) <- 
                   case (lookup (className c) nodeList2) of
                     Just(nid) -> return((nid, nodeList2))
                     Nothing -> do Result nid _ <- addnode gid "class" (className c) ginfo
                                   return((nid, nodeList ++ [((className c), nid)]))
                Result eid _ <- addlink gid "subClassOf" "" superNID subNID ginfo
                return(nodeList2 ++ [((className c), subNID)])

          Nothing -> case (lookup (className c) nodeList) of
                        Just(_) -> return(nodeList)
                        Nothing ->                        
                          do Result nid _ <- addnode gid "class" (className c) ginfo
                             return(nodeList ++ [((className c), nid)])
     insertClasses classes newNodeList gid ginfo

insertClasses [] nodeList _ _ = return(nodeList)      


insertObjects :: [ObjectDecl] -> [(String, Descr)] -> [(String, Descr)] -> Descr -> GraphInfo -> IO ([(String, Descr)])
insertObjects (o:objects) classList objList gid ginfo = 
  do
     objID <- do Result nid _ <- addnode gid "object" ("_" ++ (objName o) ++ "_") ginfo
                 return(nid)
     _ <- case (lookup (instanceOf o) classList) of
            Just(nid) -> do Result eid _ <- addlink gid "instanceOf" "" nid objID ginfo
                            return(True)      
            Nothing -> return(True)
     newObjList <- return(objList ++ [((objName o), objID)])  
     insertObjects objects classList newObjList gid ginfo

insertObjects [] _ objList _ _ = return(objList)      


insertLinks :: Bool -> [ObjectLink] -> [RelationDecl] -> [(String, Descr)] -> Descr -> GraphInfo -> IO ([(String, Descr)])
insertLinks useRelTexts (l:links) relations objList gid ginfo =   
  do
     (sourceID, objList1) <- 
        case (lookup (sourceObj l) objList) of
           Just(nid) -> return((nid, objList))
           Nothing -> do Result nid _ <- addnode gid "object" (sourceObj l) ginfo
                         return((nid, objList ++ [((sourceObj l), nid)]))    
     (targetID, objList2) <- 
        case (lookup (targetObj l) objList) of
           Just(nid) -> return((nid, objList))
           Nothing -> do Result nid _ <- addnode gid "object" (targetObj l) ginfo
                         return((nid, objList1 ++ [((targetObj l), nid)]))
     linkText <- if useRelTexts 
                   then do rel <- return (find (((linkRelation l) ==) . relName) relations)
                           case rel of
                             Just(r) -> return (relationText r)
                             Nothing -> return (linkRelation l)
                   else return (linkRelation l)    
     _ <-  do Result eid _ <- addlink gid "relation" linkText sourceID targetID ginfo
              return(True)      
     insertLinks useRelTexts links relations objList2 gid ginfo

insertLinks _ [] _ objList _ _ = return(objList)      


insertRelations :: Bool -> Bool -> [RelationDecl] -> [(String, Descr)] -> Descr -> GraphInfo -> IO ([(String, Descr)])
insertRelations insertMissingClassesOption useRelationTexts (r:rels) nodeList gid ginfo = 
  do cardStr <- case (multiplicities r) of
                  Nothing -> return ""
                  Just(str) -> return str
     (ok, sourceID, nodeList1) <- 
        case (lookup (source r) nodeList) of
           Just(nid) -> return((True, nid, nodeList))
           Nothing -> if insertMissingClassesOption 
                        then do Result nid _ <- addnode gid "class" (source r) ginfo
                                return((True, nid, nodeList ++ [((source r), nid)]))
                        else return((False, 0, nodeList))
     (ok2, targetID, nodeList2) <- 
        case (lookup (target r) nodeList1) of
           Just(nid) -> return((True, nid, nodeList1))
           Nothing -> if insertMissingClassesOption 
                        then do Result nid _ <- addnode gid "class" (target r) ginfo
                                return((True, nid, nodeList1 ++ [((target r), nid)]))    
                        else return((False, 0, nodeList))
     if (ok && ok2) 
       then do relationNameText <- if useRelationTexts then return(relationText r) else return(relName r) 
               Result eid _ <- addlink gid "relation" (relationNameText ++ "\n" ++ cardStr) sourceID targetID ginfo      
               insertRelations insertMissingClassesOption useRelationTexts rels nodeList2 gid ginfo
       else insertRelations insertMissingClassesOption useRelationTexts rels nodeList gid ginfo

insertRelations _ _ [] nodeList _ _ = return(nodeList)      


