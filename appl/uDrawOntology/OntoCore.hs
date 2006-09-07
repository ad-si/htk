{------------------------------------
MODULE        : OntoCore
AUTHOR        : Simon Drees,
                University of Bremen
DATE          : 2006
VERSION       : 1.0
DESCRIPTION   : Main file, with gui-functions for uDraw (Ontology).
-------------------------------------}

module OntoCore where

import Graphics.UI.WX
import Graphics.UI.WXCore

import qualified IO
import qualified Thread
import qualified System
import qualified List
import qualified Char

import qualified Data.Map                   as Map
import qualified Computation                as Comp
import qualified System.Environment         as SysEnv
import qualified System.Directory           as SysDir
import qualified Data.IORef                 as IORef
import qualified UDraw.Communication        as UDraw
import qualified Debug.Trace                as Debug
import qualified MMiSS.OntoParser           as OntoParser
import qualified MMiSS.MMiSSOntology        as MOnto
import qualified MMiSS.OntologyGraph        as OGraph
import qualified DaVinciTypes               as DVTypes
import qualified UDraw.Util                 as UUtil
import qualified Text.XML.HaXml.Xml2Haskell as HaXml.Xml2Haskell
import qualified UDraw.OntoSideConfig       as OntoConf
import qualified Data.Graph.Inductive       as Graph

data OntoSide = OntoSide {
        pwd               :: String,                                -- working directory
        sendIO            :: (IORef.IORef (String -> IO())),        -- chanel to send data to uDraw
        ontologyIO        :: (IORef.IORef MOnto.MMiSSOntology),     -- mmissontology
        initStringsIO     :: (IORef.IORef [String]),                -- list of init strings which
                                                                    -- have to be send on communication start
        currentGraphIO    :: (IORef.IORef (Graph.Gr (String, String, MOnto.OntoObjectType) String)),-- current graph
        currentNodeIO     :: (IORef.IORef (DVTypes.NodeId, String)),-- current selected node and its Name
        currentRelsIO     :: (IORef.IORef (UUtil.Relations)),       -- current selected relations
        nodeSettingsIO    :: (IORef.IORef (Map.Map DVTypes.NodeId (Bool, Bool, Bool))),-- current settings for each node
                                                                            -- the informations in it are: show parents,
                                                                            -- show siblings, show relations to.
  -- settings
        configIO          :: (IORef.IORef (OntoConf.Config)),
        uFontFamily       :: [String],
        uFontStyle        :: [String],
        uShape            :: [String],
        uBorder           :: [String],
        uEdgePattern      :: [String],
        uEdgeHead         :: [String],
  -- all following are wx-widgets
      -- first page
        tcFind            :: TextCtrl (),
        btnFind           :: Button (),
        cHierarchy        :: Choice (),
        tcCurrent         :: TextCtrl (),
        mlbCurrentPack    :: MultiListBox (),
        btnCurrentPackAll :: Button (),
        mlbCurrentRel     :: MultiListBox (),
        btnCurrentRelAll  :: Button (),
        cbParents         :: CheckBox (),
        cbSiblings        :: CheckBox (),
        cbInRel           :: CheckBox (),
        btnResetNodes     :: Button (),
        cbObjects         :: CheckBox (),
        cbInher           :: CheckBox (),
        btnUpdatePack     :: Button (),
        btnUpdateRel      :: Button (),
--                   slZoom            :: Slider (),
      -- second page
        choFontSClasses   :: Choice (),
        choFontFClasses   :: Choice (),
        choBorderClasses  :: Choice (),
        choShapeClasses   :: Choice (),
        choFontSObjects   :: Choice (),
        choFontFObjects   :: Choice (),
        choBorderObjects  :: Choice (),
        choShapeObjects   :: Choice (),
        slbSettingRel     :: SingleListBox (),
        btnSettingChgRel  :: Button (),
      -- general
        btnLayout         :: Button ()
{-                  tcCommand         :: ComboBox (),
                  btnExecCommand    :: Button ()-}
                }

gui :: IO ()
gui =
  do
    let uFontFamily    = ["lucida", "times", "helvetica", "courier"]
    let uFontStyle     = ["normal", "bold", "italic", "bold_italic"]
    let uShape         = ["box", "circle", "ellipse", "rhombus"]
    let uBorder        = ["none","single"]
    let uEdgePattern   = ["solid", "dotted", "dashed", "thick", "double"]
    let uEdgeHead      = ["farrow", "arrow", "oarrow", "fcircle", "circle"]
    environment <- SysEnv.getEnvironment
    pwd         <- System.getEnv "PWD"
    udgHome     <- UUtil.getUDGHOME environment
    if udgHome == ""
      then do putStrLn "OntoSide: Error, $UDG_HOME is not set."
              System.exitWith (System.ExitFailure 9)
      else Comp.done
    config     <- getConfig
    configIO   <- IORef.newIORef (config)
    IORef.writeIORef configIO config
-- main gui elements: frame, panel, text control
    f                  <- frame     [text := "uDraw (Ontology)"
                                         ,closeable := False]
    p                  <- panel        f []
    nb                 <- notebook     p []
    initStringsIO      <- IORef.newIORef []
    currentGraphIO     <- IORef.newIORef Graph.empty
    currentNodeIO      <- IORef.newIORef (DVTypes.NodeId "", "")
    currentRelsIO      <- IORef.newIORef UUtil.emptyRelations
    nodeSettingsIO     <- IORef.newIORef Map.empty
    ontologyIO <- IORef.newIORef (MOnto.emptyMMiSSOntology "empty" MOnto.AutoInsert)
    -- use IORef to make a real dialog possible
    -- Attention, the syncronisation isn't save at startup
    sendIO <- IORef.newIORef (\_ -> return ())

-- bottom page use text control as logger
    pBottom            <- panel         p             []
    btnClose           <- button        pBottom       [text    := "Close"
                                                      ,on command := onCloseOntoSide f sendIO]
    btnLayout          <- button        pBottom       [text    := "Improve layout"
                                                      ,enabled := False]
    -- only for testing
--     tcCommand          <- comboBox      pBottom    [enabled := False
--                                                    ,tooltip := "type in a udraw-command"]
--     btnExecCommand     <- button        pBottom    [enabled := False
--                                                    ,text    := "Execute Command"
--                                                    ,on command := onExecCommand f sendIO tcCommand]

-- current page
    pCurrent           <- panel         nb            []
    tcFind             <- textEntry     pCurrent      [enabled := False
                                                      ,tooltip := "type name of node to search for"
                                                      ,processEnter := True
                                                      ,wrap    := WrapNone]
    btnFind            <- button        pCurrent      [enabled := False
                                                      ,text    := "Go"]
    cHierarchy          <- choice        pCurrent     [enabled := False
                                                      ,tooltip := "choose hierarchy, ATTENTION the graph will be redrawn completely"
                                                      ,sorted := True]
    tcCurrent          <- textEntry     pCurrent      [enabled := False
                                                      ,wrap    := WrapNone]
    cbParents          <- checkBox      pCurrent      [enabled := False]
    cbSiblings         <- checkBox      pCurrent      [enabled := False]
    cbInRel            <- checkBox      pCurrent      [enabled := False]
    btnResetNodes      <- button        pCurrent      [enabled := False
                                                      ,tooltip := "This will reset all visibility-changes which have been applied."
                                                      ,text := "Reset"]
    cbObjects          <- checkBox      pCurrent      [enabled := False]
    cbInher            <- checkBox      pCurrent      [enabled := False]

    mlbCurrentPack     <- multiListBox  pCurrent      [enabled := False
                                                      ,tooltip := "choose packages, hold down [CTRL] to select more than one package"]
    mlbCurrentRel      <- multiListBox  pCurrent      [enabled := False
                                                      ,tooltip := "choose relation, hold down [CTRL] to select more than one relation"]
    btnCurrentPackAll  <- button        pCurrent      [enabled := False
                                                      ,text := "All"]
    btnCurrentRelAll   <- button        pCurrent      [enabled := False
                                                      ,text := "All"]
    btnUpdateRel       <- button        pCurrent      [enabled := False
                                                      ,text    := "Update"]
    btnUpdatePack      <- button        pCurrent      [enabled := False
                                                      ,text    := "Update"]
--     slZoom             <- hslider pCurrent True 1 100 [enabled := False
--                                                       ,text      := "Zoom"
--                                                       ,selection := 100]


-- settings page
    pSetting           <- panel         nb         []

    choFontSClasses    <- choice        pSetting   [enabled := False
                                                   ,items   := uFontStyle]
    choFontFClasses    <- choice        pSetting   [enabled := False
                                                   ,items   := uFontFamily]
    choBorderClasses   <- choice        pSetting   [enabled := False
                                                   ,items   := uBorder]
    choShapeClasses    <- choice        pSetting   [enabled := False
                                                   ,items   := uShape]
    choFontSObjects    <- choice        pSetting   [enabled := False
                                                   ,items   := uFontStyle]
    choFontFObjects    <- choice        pSetting   [enabled := False
                                                   ,items   := uFontFamily]
    choBorderObjects   <- choice        pSetting   [enabled := False
                                                   ,items   := uBorder]
    choShapeObjects    <- choice        pSetting   [enabled := False
                                                   ,items   := uShape]

    slbSettingPack     <- singleListBox pSetting   [enabled := False]
    btnSettingChgPack  <- button        pSetting   [enabled := False
                                                   ,text    := "Change"]
    slbSettingRel      <- singleListBox pSetting   [enabled := False]
    btnSettingChgRel   <- button        pSetting   [enabled := False
                                                   ,text    := "Change"]

    let ontoSide = OntoSide {
                    pwd               = pwd,
                    sendIO            = sendIO,
                    ontologyIO        = ontologyIO,
                    initStringsIO     = initStringsIO,
                    currentGraphIO    = currentGraphIO,
                    currentNodeIO     = currentNodeIO,
                    currentRelsIO     = currentRelsIO,
                    nodeSettingsIO    = nodeSettingsIO,
              -- settings
                    configIO           = configIO,
                    uFontFamily        = uFontFamily,
                    uFontStyle         = uFontStyle,
                    uShape             = uShape,
                    uBorder            = uBorder,
                    uEdgePattern       = uEdgePattern,
                    uEdgeHead          = uEdgeHead,
              -- wxWidgets
                -- first page
                    tcFind            = tcFind,
                    btnFind           = btnFind,
                    cHierarchy        = cHierarchy,
                    tcCurrent         = tcCurrent,
                    cbParents         = cbParents,
                    cbSiblings        = cbSiblings,
                    cbInRel           = cbInRel,
                    btnResetNodes     = btnResetNodes,
                    cbObjects         = cbObjects,
                    cbInher           = cbInher,
                    mlbCurrentPack    = mlbCurrentPack,
                    btnCurrentPackAll = btnCurrentPackAll,
                    mlbCurrentRel     = mlbCurrentRel,
                    btnCurrentRelAll  = btnCurrentRelAll,
                    btnUpdatePack     = btnUpdatePack,
                    btnUpdateRel      = btnUpdateRel,
--                     slZoom            = slZoom,
                -- second page
                    choFontSClasses  = choFontSClasses,
                    choFontFClasses  = choFontFClasses,
                    choBorderClasses = choBorderClasses,
                    choShapeClasses  = choShapeClasses,
                    choFontSObjects  = choFontSObjects,
                    choFontFObjects  = choFontFObjects,
                    choBorderObjects = choBorderObjects,
                    choShapeObjects  = choShapeObjects,
                    slbSettingRel    = slbSettingRel,
                    btnSettingChgRel = btnSettingChgRel,
                -- general
                    btnLayout        = btnLayout
{-                    tcCommand         = tcCommand,
                    btnExecCommand    = btnExecCommand-}
                    }
-- start uDraw
    (send,process,pid) <- processExecAsync f (uDrawBin udgHome) 256 (onEndProcess f)
                                                                    (onReceive f ontoSide) (onReceive f ontoSide)
    IORef.writeIORef sendIO send
    IORef.writeIORef initStringsIO UDraw.initOntoToolStrings

-- layout
    set f [layout := container p $ margin 10 $ column 10
            [hfill $ tabs nb
              [tab "Current" $ container pCurrent $ margin 5 $ column 5
                [grid 5 5 [[empty, hspace 255, empty]
                          ,[valignCenter $ label "Find: "
                           ,valignCenter $ hfill $ widget tcFind
                           ,valignCenter $ widget btnFind]]
                ,grid 5 5 [[valignCenter $ label "Hierarchy: "
                           ,valignCenter $ hfill $ widget cHierarchy]]
                ,boxed "Node" (grid 5 5
                  [[valignCenter $ label "Current: "
                   ,valignCenter $ hfill $ widget tcCurrent]
                  ,[valignCenter $ label "Show parents: "
                   ,row 5 [valignCenter $ hfill $ widget cbParents
                          ,valignCenter $ label "Show siblings: ",valignCenter $ hfill $ widget cbSiblings]]
                  ,[valignCenter $ label "In relation: "
                   ,row 5 [valignCenter $ hfill $ widget cbInRel
                          ,valignCenter $ widget btnResetNodes]]])
                ,boxed "Nodes" (grid 5 5
                  [[valignCenter $ label "Show objects: "
                   ,valignCenter $ hfill $ widget cbObjects
                   ,valignCenter $ label "Show inheritance: "
                   ,valignCenter $ hfill $ widget cbInher]])
                ,row 5 [floatLeft $ expand $ boxed "Packages" (grid 0 0
                         [[empty, hspace 150]
                         ,[vspace 135, fill $ widget mlbCurrentPack]
                         ,[vspace 5, empty]
                         ,[empty, row 5 [widget btnCurrentPackAll, widget btnUpdatePack]]])
                       ,floatRight $ expand $ boxed "Relations" (grid 0 0
                         [[empty, hspace 150]
                         ,[vspace 135, fill $ widget mlbCurrentRel]
                         ,[vspace 5, empty]
                         ,[empty, row 5 [widget btnCurrentRelAll, widget btnUpdateRel]]])]
                 {-, hfill $ widget slZoom-}]
              ,tab "Settings" $ container pSetting $ margin 5 $ column 5
                 [boxed "Classes" (grid 5 5
                    [[valignCenter $ label "Font-Style: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choFontSClasses
                     ,hspace 30
                     ,valignCenter $ label "Font-Family: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choFontFClasses]
                    ,[valignCenter $ label "Border: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choBorderClasses
                     ,hspace 30
                     ,valignCenter $ label "Shape: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choShapeClasses]])
                 ,boxed "Objects" (grid 5 5
                    [[valignCenter $ label "Font-Style: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choFontSObjects
                     ,hspace 30
                     ,valignCenter $ label "FontFamily: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choFontFObjects]
                    ,[valignCenter $ label "Border: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choBorderObjects
                     ,hspace 30
                     ,valignCenter $ label "Shape: "
                     ,valignCenter $ minsize (sz 85 25) $ widget choShapeObjects]])
              ,row 5 [floatLeft $ expand $ boxed "Packages" (grid 0 0
                       [[empty, hspace 150]
                       ,[vspace 165, fill $ widget slbSettingPack]
                       ,[vspace 5, empty]
                       ,[empty, row 5 [widget btnSettingChgPack]]])
                     ,floatRight $ expand $ boxed "Relations" (grid 0 0
                       [[empty, hspace 150]
                       ,[vspace 165, fill $ widget slbSettingRel]
                       ,[vspace 5, empty]
                       ,[empty, row 5 [widget btnSettingChgRel]]])]]]
            ,container pBottom  $ column 5
              [row 5[floatLeft $ widget btnLayout, floatRight $ widget btnClose]]
          {-,container pBottom  $ column 5
              [hfill $ widget tcCommand
              ,row 5 [widget btnExecCommand, floatRight $ widget btnClose]]-}]]
    return ()
    where
  -- create command to start uDraw... dopey
    uDrawBin :: String -> String
    uDrawBin udgHome = udgHome ++ "/bin/uDrawGraph -pipe -nogui"
  -- get configuration
  -- check if configfile exist and if not, create one
    getConfig :: IO (OntoConf.Config)
    getConfig =
      do pwd          <- System.getEnv "PWD"
         home         <- System.getEnv "HOME"
         let configFile = (home ++ "/.ontoside.xml")
         let baseconfig = (pwd ++ "/resource/ontoside.xml")
         let newconfig  = (pwd  ++ "/resource/newontoside.xml")
         configExist  <- SysDir.doesFileExist (configFile)
         newconfExist <- SysDir.doesFileExist (newconfig)
         if (newconfExist)
          then do putStrLn "OntoSide: a new config-file exist, going to use it..."
                  SysDir.copyFile (newconfig) (configFile)
                  SysDir.removeFile (newconfig)
                  Comp.done
          else do if (not configExist)
                    then do putStrLn "OntoSide: config-file does not exist, going to create one..."
                            SysDir.copyFile (baseconfig) (configFile)
                            Comp.done
                    else do putStrLn "OntoSide: config-file does exist, going to read it..."
                            Comp.done
         config <- HaXml.Xml2Haskell.fReadXml (configFile)
         return (config)

--------------------------------------------
-- test
--------------------------------------------
onExecCommand :: (Closeable frame, Textual comboBox) => frame -> (IORef.IORef (String -> IO())) -> comboBox -> IO ()
onExecCommand f sendIO tcCommand =
  do  send <- IORef.readIORef sendIO
      cmd  <- get tcCommand text
      appendText tcCommand cmd
      Debug.trace ("OntoSide: " ++ cmd) (send (cmd ++ "\n"))

--------------------------------------------
-- called when OntoSide close button was 
-- pressed
--------------------------------------------
onCloseOntoSide :: (Closeable frame) => frame -> (IORef.IORef (String -> IO())) -> IO ()
onCloseOntoSide f sendIO =
  do  send <- IORef.readIORef sendIO
      Debug.trace "OntoSide: onCloseOntoSide is called" (send UDraw.exit)
      -- we have to wait, so uDraw can close
      Thread.delay (Thread.secs 0.2)

--------------------------------------------
-- called when frame is going to close
--------------------------------------------
onClosing :: (String -> IO()) -> IO ()
onClosing send =
  do  Debug.trace "OntoSide: onClosing is called" (send UDraw.exit)
      -- we have to wait, so uDraw can close
      Thread.delay (Thread.secs 0.2)
      propagateEvent

--------------------------------------------
-- called when uDraw-process ends
--------------------------------------------
onEndProcess :: (Closeable frame) => frame -> Int -> IO ()
onEndProcess f exitcode =
  do  -- we have to wait, so uDraw can close
      Thread.delay (Thread.secs 0.2)
      Debug.trace "OntoSide: onEndProcess is called" (close f)

--------------------------------------------
-- called each time the uDraw-process sends
-- data
-- We have to check the streamstatus, because
-- when uDraw is going to close, you get 
-- scrabled messages, which we are not 
-- interessted in. If we parse them, we get
-- problems with a memory leak.
--------------------------------------------
onReceive :: (Closeable (Frame a)) => Frame a -> OntoSide -> String -> StreamStatus -> IO ()
onReceive f ontoSide message streamstatus =
  do  case streamstatus of 
        StreamOk -> do  putStr ("uDraw: " ++ message)
                        send <- IORef.readIORef (sendIO ontoSide)
                        initStrings <- IORef.readIORef (initStringsIO ontoSide)
                        handleMessage f ontoSide message
                        -- the first OK indicates, that uDraw has been started, now we can send our initStrings
                        if (initStrings == [])
                          then Comp.done
                          else do Debug.trace ("OntoSide: " ++ head initStrings) (send ((head initStrings) ++ "\n"))
                                  IORef.writeIORef (initStringsIO ontoSide) (drop 1 initStrings)
                                  Comp.done
        _        -> Comp.done

--------------------------------------------
-- handle incoming messages
--------------------------------------------
handleMessage :: (Closeable (Frame a)) => Frame a -> OntoSide -> String -> IO ()
handleMessage f ontoSide message =
  do  case getAnswer message of
        DVTypes.Ok                            -> do Comp.done                                       -- everything is fine, nothing to be done
        DVTypes.CommunicationError message    -> do onCommunicationError f ontoSide message
        DVTypes.Versioned _                   -> do putStrLn "still to handle"
        DVTypes.NodeSelectionsLabels labels   -> do onNodeSelectionLabels f ontoSide labels
        DVTypes.NodeDoubleClick               -> do putStrLn "still to handle"
        DVTypes.EdgeSelectionLabel _          -> do putStrLn "still to handle"
        DVTypes.EdgeSelectionLabels _ _       -> do putStrLn "still to handle"
        DVTypes.EdgeDoubleClick               -> do putStrLn "still to handle"
        DVTypes.MenuSelection mId             -> do onMenuSelection f ontoSide mId
        DVTypes.IconSelection _               -> do putStrLn "still to handle"
        DVTypes.Context _                     -> do putStrLn "still to handle"
        DVTypes.TclAnswer _                   -> do putStrLn "still to handle"
        DVTypes.BrowserAnswer filename _      -> if (filename == "") then Comp.done else do onBrowserAnswer f ontoSide filename
        DVTypes.Disconnect                    -> do putStrLn "still to handle"
        DVTypes.Closed                        -> do putStrLn "still to handle"
        DVTypes.Quit                          -> do Comp.done                            -- everything is fine, nothing to be done
        DVTypes.PopupSelectionNode nId handle -> do onPopupSelectionNode f ontoSide nId handle
        DVTypes.PopupSelectionEdge eId handle -> do onPopupSelectionEdge f ontoSide eId handle
        DVTypes.CreateNode                    -> do putStrLn "still to handle"
        DVTypes.CreateNodeAndEdge _           -> do putStrLn "still to handle"
        DVTypes.CreateEdge _ _                -> do putStrLn "still to handle"
        DVTypes.DropNode _ _ _ _              -> do putStrLn "still to handle"
        DVTypes.ContextWindow _ _             -> do putStrLn "still to handle"
        DVTypes.OpenWindow                    -> do putStrLn "still to handle"
        DVTypes.CloseWindow _                 -> do putStrLn "still to handle"

--------------------------------------------
-- transform answer-strings t
-- DaVinciAnswers
--------------------------------------------
getAnswer :: String -> DVTypes.DaVinciAnswer
getAnswer message = read message

--------------------------------------------
-- is called everytime a communicationerror
-- occurred. This is a real Problem and
-- should never happen.
--------------------------------------------
onCommunicationError :: frame -> OntoSide -> String -> IO ()
onCommunicationError f ontoSide message =
  do send <- IORef.readIORef (sendIO ontoSide)
     putStrLn ("A c ommunication error occurred with this problem: " ++ message)
     Comp.done

--------------------------------------------
-- is called whenever a node is selected.
-- ATTENTION! At the moment, we only handle
-- the first selected node.
--------------------------------------------
onNodeSelectionLabels :: frame -> OntoSide -> [DVTypes.NodeId] -> IO ()
onNodeSelectionLabels f ontoSide [] =
  do  IORef.writeIORef (currentNodeIO ontoSide) (DVTypes.NodeId "", "")
      set (tcCurrent ontoSide)    [text := ""]
      set (cbParents ontoSide)    [enabled := False
                                  ,checked := False]
      set (cbSiblings ontoSide)   [enabled := False
                                  ,checked := False]
      set (cbInRel ontoSide)      [enabled := False
                                  ,checked := False]
onNodeSelectionLabels f ontoSide (nId:rest) =
  do  onto <- IORef.readIORef (ontologyIO ontoSide)
      nodeSettings <- IORef.readIORef (nodeSettingsIO ontoSide)
      let nName = OGraph.getNodeName nId onto
      IORef.writeIORef (currentNodeIO ontoSide) (nId, nName)
      set (tcCurrent ontoSide)    [text := (nName)]
      set (cbParents ontoSide)    [enabled := True]
      set (cbSiblings ontoSide)   [enabled := True]
      set (cbInRel ontoSide)      [enabled := True]
      case Map.lookup nId nodeSettings of
        Nothing -> do set (cbParents ontoSide)    [checked := False]
                      set (cbSiblings ontoSide)   [checked := False]
                      set (cbInRel ontoSide)      [checked := False]
        Just (shPar,shSib,shRel) -> do set (cbParents ontoSide)    [checked := shPar]
                                       set (cbSiblings ontoSide)   [checked := shSib]
                                       set (cbInRel ontoSide)      [checked := shRel]
      Comp.done

--------------------------------------------
-- is called everytime a menuitem was
-- selected
--------------------------------------------
onMenuSelection :: (Closeable frame) => frame -> OntoSide -> DVTypes.MenuId -> IO ()
onMenuSelection f ontoSide (DVTypes.MenuId ('#':'%':fileMenuStr)) =
  do send <- IORef.readIORef (sendIO ontoSide)
     if (fileMenuStr == "close" || fileMenuStr == "exit")
        then do Debug.trace "OntoSide: now closing uDraw" (send UDraw.exit)
        else Comp.done
     if (fileMenuStr == "open")
        then do send (UDraw.openFileDialog (pwd ontoSide))
        else Comp.done

--------------------------------------------
-- is called when the user wants to open a
-- file.
--------------------------------------------
onBrowserAnswer :: Frame a -> OntoSide -> String -> IO ()
onBrowserAnswer f ontoSide filename =
    do  send   <- IORef.readIORef (sendIO ontoSide)
        config <- IORef.readIORef (configIO ontoSide)
        home   <- System.getEnv "HOME"
        putStrLn ("going to open file: " ++ filename)
        ontofWE <- OntoParser.parseMMiSSOntologyFile filename
        onto    <- case Comp.fromWithError ontofWE of
                    Left message -> do let str = "The following errors occured during parsing:\n"
                                       onError (str ++ message)
                                       return (MOnto.emptyMMiSSOntology "OntoSideErrorOnLoading" MOnto.AutoInsert)
                    Right o -> let messages = MOnto.isComplete o
                               in if (messages == [])
                                    then Debug.trace "Parse: Successfull\nChecking Ontology: Successfull\n" (return o)
                                    else Debug.trace (unlines messages) (return o)
        config2 <- checkConfig onto config
        case config2 of
           Nothing -> do Comp.done
           Just(conf) -> do HaXml.Xml2Haskell.fWriteXml ((pwd ontoSide) ++ "/resource/newontoside.xml") conf
                            IORef.writeIORef (configIO ontoSide) conf
        currentConfig <- IORef.readIORef (configIO ontoSide)
        let classGraph  = MOnto.getClassGraph onto
        let graphString = OGraph.createGraph classGraph currentConfig
        IORef.writeIORef (nodeSettingsIO ontoSide) Map.empty
        IORef.writeIORef (ontologyIO ontoSide) onto
        IORef.writeIORef (currentGraphIO ontoSide) (MOnto.getClassGraph onto)
        Debug.trace (graphString) (send graphString)
        if ((MOnto.getOntologyName onto) == "OntoSideErrorOnLoading") then Comp.done else resetGUI f ontoSide
        Comp.done
        where
        onError message =
          do warningDialog f "Error on loading file" message

--------------------------------------------
-- is called whenever a function on a node
-- is selected.
--------------------------------------------
onPopupSelectionNode :: frame -> OntoSide -> DVTypes.NodeId -> DVTypes.MenuId -> IO ()
onPopupSelectionNode f ontoSide nId (DVTypes.MenuId (fileMenuStr)) =
  do send <- IORef.readIORef (sendIO ontoSide)
     nodeSettings <- IORef.readIORef (nodeSettingsIO ontoSide)
     let (shPar,shSib,inRel) = case Map.lookup nId nodeSettings of
                                 Nothing -> (False,False,False)
                                 Just (x,y,z) -> (x,y,z)
     let newNodeSettings = case fileMenuStr of
                              "shPar" -> if ((not shPar) || shSib || inRel)
                                           then Map.insert nId ((not shPar),shSib,inRel) nodeSettings
                                           else Map.delete nId nodeSettings
                              "shSib" -> if (shPar || (not shSib) || inRel)
                                           then Map.insert nId (shPar,(not shSib),inRel) nodeSettings
                                           else Map.delete nId nodeSettings
                              "inRel" -> if (shPar || shSib || (not inRel))
                                           then Map.insert nId (shPar,shSib,(not inRel)) nodeSettings
                                           else Map.delete nId nodeSettings
                              _       -> nodeSettings
     IORef.writeIORef (nodeSettingsIO ontoSide) newNodeSettings
     updateGraph ontoSide

-------------------------------------------
-- is called whenever a function on an edge
-- is selected.
-- ToDo: not yet finished
--------------------------------------------
onPopupSelectionEdge :: frame -> OntoSide -> DVTypes.EdgeId -> DVTypes.MenuId -> IO ()
onPopupSelectionEdge f ontoSide eId (DVTypes.MenuId (fileMenuStr)) = 
  do case fileMenuStr of
      "hideRel" -> do send <- IORef.readIORef (sendIO ontoSide)
                      oldRelations <- IORef.readIORef (currentRelsIO ontoSide)
                      let selRels = UUtil.getSelectedRelations oldRelations
                      let newRelations = case (UUtil.getRelationIndex oldRelations (filter Char.isAlpha (show eId))) of
                                              Nothing -> oldRelations
                                              Just ind -> UUtil.setSelectedRelations oldRelations (filter (/= ind) selRels)
                      IORef.writeIORef (currentRelsIO ontoSide) newRelations
                      set (mlbCurrentRel ontoSide) [selections := UUtil.getSelectedRelations newRelations]
      _          -> Comp.done
     updateGraph ontoSide

--------------------------------------------
-- on hierarchy selection, redraws a
-- complete new graph with only the one
-- selected relation.
--------------------------------------------
onHierarchySelection :: frame -> OntoSide -> IO ()
onHierarchySelection f ontoSide =
  do  oldSelectedRels       <- IORef.readIORef (currentRelsIO ontoSide)
      send                  <- IORef.readIORef (sendIO ontoSide)
      selectedRelationIndex <- get (cHierarchy ontoSide) selection
      currentConfig         <- IORef.readIORef (configIO ontoSide)
      onto                  <- IORef.readIORef (ontologyIO ontoSide)
      let classGraph  = MOnto.getClassGraph onto
      let newSelectedRels = UUtil.setSelectedRelations oldSelectedRels [selectedRelationIndex]
      IORef.writeIORef (currentRelsIO ontoSide) newSelectedRels
   -- select relation in listBox
      set (mlbCurrentRel ontoSide)    [selections := UUtil.getSelectedRelations newSelectedRels]
      let newGraph = (OGraph.filterRelationsNotX (head (UUtil.getSelectedRelationNames newSelectedRels)) classGraph)
      let graphString = OGraph.createGraph newGraph currentConfig
      IORef.writeIORef (currentGraphIO ontoSide) newGraph
      Debug.trace (graphString) (send graphString)

--------------------------------------------
-- this will reset the GUI to its default
-- values, depending on loaded ontology.
-- CODEREFACTORING IS REQUIRED!!!!
--------------------------------------------
resetGUI :: Frame a -> OntoSide -> IO ()
resetGUI f ontoSide =
  do  onto <- IORef.readIORef (ontologyIO ontoSide)
      config <- IORef.readIORef (configIO ontoSide)
      selectedRels <- IORef.readIORef (currentRelsIO ontoSide)
      let selectedRels = UUtil.setUpRelations ("isa":(relations onto))
      let OntoConf.Config nodes rels packs = config
      IORef.writeIORef (currentRelsIO ontoSide) selectedRels
  -- first page (current)
      set (tcFind ontoSide)           [enabled := True
                                      ,on command := findNodes f ontoSide]
      set (btnFind ontoSide)          [enabled := True
                                      ,on command := findNodes f ontoSide]
      set (cHierarchy ontoSide)       [enabled := True
                                      ,items := (List.sort (UUtil.getRelationNames selectedRels))
                                      ,on select := onHierarchySelection f ontoSide]
      set (tcCurrent ontoSide)        [enabled := False, text := ""]
      set (cbParents ontoSide)        [enabled := False
                                      ,checked := False
                                      ,on command := onNodeSettings "parents" (cbParents ontoSide) ontoSide]
      set (cbSiblings ontoSide)       [enabled := False
                                      ,checked := False
                                      ,on command := onNodeSettings "siblings" (cbSiblings ontoSide) ontoSide]
      set (cbInRel ontoSide)          [enabled := False
                                      ,checked := False
                                      ,on command := onNodeSettings "inRel" (cbInRel ontoSide) ontoSide]
      set (btnResetNodes ontoSide)    [enabled := True
                                      ,on command := onResetNodes ontoSide]
      set (cbObjects ontoSide)        [enabled := True
                                      ,checked := True
                                      ,on command := updateGraph ontoSide]
  -- ToDo: handle Packages!!!!
      set (mlbCurrentPack ontoSide)   [enabled := False]
      set (btnCurrentPackAll ontoSide)[enabled := False]
      set (mlbCurrentRel ontoSide)    [enabled := True
                                      ,items := (List.sort (UUtil.getRelationNames selectedRels))
                                      ,selections := UUtil.getSelectedRelations selectedRels]
      set (btnUpdateRel ontoSide)     [enabled := True
                                      ,on command := updateGraph ontoSide]
      set (btnCurrentRelAll ontoSide) [enabled := True
                                      ,on command := onBtnCurrentRelAll ontoSide]
{-      set (slZoom ontoSide)           [enabled := True
                                      ,on command := onZoom ontoSide]-}
  -- second page (settings)
      set (choFontSClasses ontoSide)  [enabled := True
                                      ,selection := (getPos (getFontStyle  (getObject "class" nodes))  (uFontStyle ontoSide)  0)
                                      ,on select := updGraphAttribAndConf "fontStyle" "class" (choFontSClasses ontoSide) ontoSide]
      set (choFontFClasses ontoSide)  [enabled := True
                                      ,selection := (getPos (getFontFamily (getObject "class" nodes))  (uFontFamily ontoSide) 0)
                                      ,on select := updGraphAttribAndConf "fontFamily" "class"(choFontFClasses ontoSide) ontoSide]
      set (choBorderClasses ontoSide) [enabled := True
                                      ,selection := (getPos (getBorder     (getObject "class" nodes))  (uBorder ontoSide)     0)
                                      ,on select := updGraphAttribAndConf "border" "class" (choBorderClasses ontoSide) ontoSide]
      set (choShapeClasses ontoSide)  [enabled := True
                                      ,selection := (getPos (getShape      (getObject "class" nodes))  (uShape ontoSide)      0)
                                      ,on select := updGraphAttribAndConf "shape" "class" (choShapeClasses ontoSide) ontoSide]
      set (choFontSObjects ontoSide)  [enabled := True
                                      ,selection := (getPos (getFontStyle  (getObject "object" nodes)) (uFontStyle ontoSide)  0)
                                      ,on select := updGraphAttribAndConf "fontStyle" "object" (choFontSObjects ontoSide) ontoSide]
      set (choFontFObjects ontoSide)  [enabled := True
                                      ,selection := (getPos (getFontFamily (getObject "object" nodes)) (uFontFamily ontoSide) 0)
                                      ,on select := updGraphAttribAndConf "fontFamily" "object" (choFontFObjects ontoSide) ontoSide]
      set (choBorderObjects ontoSide) [enabled := True
                                      ,selection := (getPos (getBorder     (getObject "object" nodes)) (uBorder ontoSide)     0)
                                      ,on select := updGraphAttribAndConf "border" "object" (choBorderObjects ontoSide) ontoSide]
      set (choShapeObjects ontoSide)  [enabled := True
                                      ,selection := (getPos (getShape      (getObject "object" nodes)) (uShape ontoSide)      0)
                                      ,on select := updGraphAttribAndConf "shape" "object" (choShapeObjects ontoSide) ontoSide]
      set (slbSettingRel ontoSide)    [enabled := True, items := (List.sort ("isa":(relations onto)))
                                      ,on select := set (btnSettingChgRel ontoSide) [enabled := True]]
      set (btnSettingChgRel ontoSide) [on command := do
                                          i <- get (slbSettingRel ontoSide) selection
                                          s <- get (slbSettingRel ontoSide) (item i)
                                          changeRelationSettings f s ontoSide]
  -- general
      set (btnLayout ontoSide)        [enabled := True
                                      ,on command := onImproveLayout ontoSide]
{-      set (tcCommand ontoSide) [enabled := True]
      set (btnExecCommand ontoSide) [enabled := True]-}
      focusOn (tcFind ontoSide)
      Comp.done
  where
--   onZoom :: OntoSide -> IO ()
--   onZoom ontoSide = do
--     send <- IORef.readIORef (sendIO ontoSide)
--     i    <- get (slZoom ontoSide) selection
--     Debug.trace ("OntoSide: " ++ (UDraw.zoom i)) (send (UDraw.zoom i))
  onImproveLayout :: OntoSide -> IO ()
  onImproveLayout ontoSide = do
    send <- IORef.readIORef (sendIO ontoSide)
    send UDraw.improveLayout

  onResetNodes :: OntoSide -> IO ()
  onResetNodes ontoSide = do
    IORef.writeIORef (nodeSettingsIO ontoSide) Map.empty
    set (cbParents ontoSide)    [checked := False]
    set (cbSiblings ontoSide)   [checked := False]
    set (cbInRel ontoSide)      [checked := False]
    updateGraph ontoSide
    return ()
 --
  onBtnCurrentRelAll :: OntoSide -> IO ()
  onBtnCurrentRelAll ontoSide = do
    selectedRels <- IORef.readIORef (currentRelsIO ontoSide)
    set (mlbCurrentRel ontoSide) [selections := [0..(length (UUtil.getRelationNames selectedRels))]]
    return ()
 --
  updGraphAttribAndConf :: (Selection w, Items w String) => String -> String -> w -> OntoSide -> IO ()
  updGraphAttribAndConf settingType objectType w ontoSide = do
    i <- get w selection
    s <- get w (item i)
    config <- IORef.readIORef (configIO ontoSide)
    let OntoConf.Config nodes rels packs = config
    let config2 = case settingType of
                    "fontFamily" -> OntoConf.Config (setObjectFontFamily objectType nodes s) rels packs
                    "fontStyle"  -> OntoConf.Config (setObjectFontStyle  objectType nodes s) rels packs
                    "border"     -> OntoConf.Config (setObjectBorder     objectType nodes s) rels packs
                    "shape"      -> OntoConf.Config (setObjectShape      objectType nodes s) rels packs
    IORef.writeIORef (configIO ontoSide) config2
    HaXml.Xml2Haskell.fWriteXml ((pwd ontoSide) ++ "/resource/newontoside.xml") config2
    updateGraphAttributes ontoSide
    return ()
  --
  relations onto =
    map extractRelationName (MOnto.getRelations onto)
    where
    extractRelationName (name, _, _, _) = name
  --
  getPos :: String -> [String] -> Int -> Int
  getPos pattern [] pos = 0                 -- for the impossible case, that there is no match
  getPos pattern (x:rest) pos = if pattern == x then pos else getPos pattern rest (pos+1)
  --
  getFontStyle :: OntoConf.Node -> String
  getFontStyle node = (OntoConf.nodeFontStyle node)
  --
  getFontFamily :: OntoConf.Node -> String
  getFontFamily node = (OntoConf.nodeFontFamily node)
  --
  getBorder :: OntoConf.Node -> String
  getBorder node = (OntoConf.nodeBorder node)
  --
  getShape :: OntoConf.Node -> String
  getShape node = (OntoConf.nodeShape node)
  --
  getObject :: String -> OntoConf.Nodes -> OntoConf.Node
  getObject searchString (OntoConf.Nodes nodes) =
    foldl1 isObject nodes
    where
    isObject x y
        | (OntoConf.nodeType x) == searchString = x
        | otherwise = y
  --
  setObjectFontStyle :: String -> OntoConf.Nodes -> String -> OntoConf.Nodes
  setObjectFontStyle searchString (OntoConf.Nodes nodes) newValue = (OntoConf.Nodes (map (update searchString newValue) nodes))
    where
    update searchString newValue node = if (OntoConf.nodeType node) == searchString
                                          then OntoConf.Node { OntoConf.nodeType       = (OntoConf.nodeType node),
                                                               OntoConf.nodeFontStyle  = newValue,
                                                               OntoConf.nodeFontFamily = (OntoConf.nodeFontFamily node),
                                                               OntoConf.nodeBorder     = (OntoConf.nodeBorder node),
                                                               OntoConf.nodeShape      = (OntoConf.nodeShape node)}
                                          else node
  setObjectFontFamily :: String -> OntoConf.Nodes -> String -> OntoConf.Nodes
  setObjectFontFamily searchString (OntoConf.Nodes nodes) newValue = (OntoConf.Nodes (map (update searchString newValue) nodes))
    where
    update searchString newValue node = if (OntoConf.nodeType node) == searchString
                                          then OntoConf.Node { OntoConf.nodeType       = (OntoConf.nodeType node),
                                                               OntoConf.nodeFontStyle  = (OntoConf.nodeFontStyle node),
                                                               OntoConf.nodeFontFamily = newValue,
                                                               OntoConf.nodeBorder     = (OntoConf.nodeBorder node),
                                                               OntoConf.nodeShape      = (OntoConf.nodeShape node)}
                                          else node
  setObjectBorder :: String -> OntoConf.Nodes -> String -> OntoConf.Nodes
  setObjectBorder searchString (OntoConf.Nodes nodes) newValue = (OntoConf.Nodes (map (update searchString newValue) nodes))
    where
    update searchString newValue node = if (OntoConf.nodeType node) == searchString
                                          then OntoConf.Node { OntoConf.nodeType       = (OntoConf.nodeType node),
                                                               OntoConf.nodeFontStyle  = (OntoConf.nodeFontStyle node),
                                                               OntoConf.nodeFontFamily = (OntoConf.nodeFontFamily node),
                                                               OntoConf.nodeBorder     = newValue,
                                                               OntoConf.nodeShape      = (OntoConf.nodeShape node)}
                                          else node

  setObjectShape :: String -> OntoConf.Nodes -> String -> OntoConf.Nodes
  setObjectShape searchString (OntoConf.Nodes nodes) newValue = (OntoConf.Nodes (map (update searchString newValue) nodes))
    where
    update searchString newValue node = if (OntoConf.nodeType node) == searchString
                                          then OntoConf.Node { OntoConf.nodeType       = (OntoConf.nodeType node),
                                                               OntoConf.nodeFontStyle  = (OntoConf.nodeFontStyle node),
                                                               OntoConf.nodeFontFamily = (OntoConf.nodeFontFamily node),
                                                               OntoConf.nodeBorder     = (OntoConf.nodeBorder node),
                                                               OntoConf.nodeShape      = newValue}
                                          else node

--------------------------------------------
-- Called each time settings from node 
-- are changed.
--------------------------------------------
onNodeSettings :: (Checkable w) => String -> w -> OntoSide -> IO ()
onNodeSettings settingType w ontoSide = do
  (nId,_) <- IORef.readIORef (currentNodeIO ontoSide)
  nodeSettings <- IORef.readIORef (nodeSettingsIO ontoSide)
  checked <- get w checked
  if checked
    then do
      let (shPar,shSib,shRel) = case Map.lookup nId nodeSettings of
                                  Nothing -> (False,False,False)
                                  Just (x,y,z) -> (x,y,z)
      let newNodeSettings = case settingType of
                              "parents"  -> Map.insert nId (checked,shSib,shRel) nodeSettings
                              "siblings" -> Map.insert nId (shPar,checked,shRel) nodeSettings
                              "inRel"    -> Map.insert nId (shPar,shSib,checked) nodeSettings
      IORef.writeIORef (nodeSettingsIO ontoSide) newNodeSettings
      updateGraph ontoSide
    else do
      let newNodeSettings = case Map.lookup nId nodeSettings of
                              Nothing -> nodeSettings
                              Just (shPar,shSib,shRel) -> case settingType of
                                "parents"  -> if (shSib || shRel)
                                                then Map.insert nId (checked,shSib,shRel) nodeSettings
                                                else Map.delete nId nodeSettings
                                "siblings" -> if (shPar || shRel)
                                                then Map.insert nId (shPar,checked,shRel) nodeSettings
                                                else Map.delete nId nodeSettings
                                "inRel"    -> if (shPar || shSib)
                                                then Map.insert nId (shPar,shSib,checked) nodeSettings
                                                else Map.delete nId nodeSettings
      IORef.writeIORef (nodeSettingsIO ontoSide) newNodeSettings
      updateGraph ontoSide
  return ()

--------------------------------------------
-- this will check if every relation (ToDo:
-- and Package) have a configuration. If not
-- a sample will be generated and stored in
-- configurationfile
-- CODEREFACTORING IS REQUIRED!!!!
-- ToDo: handle Packages
--------------------------------------------
checkConfig :: MOnto.MMiSSOntology -> OntoConf.Config -> IO (Maybe (OntoConf.Config))
checkConfig onto config = do
  let OntoConf.Config nodes (OntoConf.Relations rels) packs = config
  let classGraph  = MOnto.getClassGraph onto
  let (rels2, isRelChanged) = checkRelations (Graph.labEdges classGraph) rels False
  if isRelChanged 
    then return (Just (OntoConf.Config nodes (OntoConf.Relations rels2) packs))
    else return Nothing
  where
  checkRelations [] rels isRelChanged = (rels, isRelChanged)
  checkRelations ((_,_,relName):rest) rels isRelChanged =
    if isInRelations relName rels 
      then checkRelations rest rels (isRelChanged || False) 
      else checkRelations rest ((createEntry relName):rels) True
    where
    isInRelations :: String -> [OntoConf.Relation] -> Bool
    isInRelations relName rels =
      any (isIt relName) rels
      where
      isIt :: String -> OntoConf.Relation -> Bool
      isIt relName (OntoConf.Relation rLabel _ _ _ _ _ _) = relName == rLabel
    createEntry :: String -> OntoConf.Relation
    createEntry relName = OntoConf.Relation { OntoConf.relationLabel       = relName,
                                              OntoConf.relationShowLabel   = "true",
                                              OntoConf.relationFontStyle   = "bold",
                                              OntoConf.relationFontFamily  = "lucida",
                                              OntoConf.relationColor       = "#ffffff",
                                              OntoConf.relationEdgePattern = "solid",
                                              OntoConf.relationHead        = "farrow"}

--------------------------------------------
-- this will filter the graph and than
-- update it.
-- The filter will always running on the
-- complete graph, never on an allready
-- filtered graph.
--------------------------------------------
updateGraph :: OntoSide -> IO ()
updateGraph ontoSide = do
  send            <- IORef.readIORef (sendIO ontoSide)
  onto            <- IORef.readIORef (ontologyIO ontoSide)
  config          <- IORef.readIORef (configIO ontoSide)
  oldRels         <- IORef.readIORef (currentRelsIO ontoSide)
  currentGraph    <- IORef.readIORef (currentGraphIO ontoSide)
  nodeSettings    <- IORef.readIORef (nodeSettingsIO ontoSide)
  showObjects     <- get (cbObjects ontoSide) checked
  selRelations    <- get (mlbCurrentRel ontoSide) selections
  let currentRels = UUtil.setSelectedRelations oldRels selRelations
  let classGraph  = MOnto.getClassGraph onto
 -- reduce objects
  let cGPhase1    = if showObjects
                      then classGraph
                      else OGraph.filterObjects classGraph
 -- reduce nodes (parents,siblings,in relation)
  let cGPhase2    = if (not (Map.null nodeSettings))
                      then
                      let nodeList = getNodesToDelete (Map.toList nodeSettings) (OGraph.filterRelationsNotX "isa" cGPhase1) cGPhase1
                      in Graph.delNodes nodeList cGPhase1
                      else
                      cGPhase1
 -- reduce relations
  let newGraph    = OGraph.filterRelations currentRels cGPhase2
  Debug.trace ("OntoSide: " ++ (OGraph.updateGraph currentGraph onto newGraph config))
                               (send (OGraph.updateGraph currentGraph onto newGraph config))
  IORef.writeIORef (currentGraphIO ontoSide) newGraph
  return ()
  where
   -- for performance reasons we create the isa-graph only once
    getNodesToDelete :: [(DVTypes.NodeId, (Bool, Bool, Bool))]-> Graph.Gr (String, String, MOnto.OntoObjectType) String
                          -> Graph.Gr (String, String, MOnto.OntoObjectType) String -> [Int]
    getNodesToDelete nodeSettings isaGraph graph =
      let nodesToStay = List.sort (getNodesToStay nodeSettings isaGraph graph [])
          nodes       = Graph.nodes graph
      in Debug.trace (show nodesToStay) (removeNodes nodes nodesToStay [])
  --
    getNodesToStay :: [(DVTypes.NodeId, (Bool, Bool, Bool))]-> Graph.Gr (String, String, MOnto.OntoObjectType) String
                        -> Graph.Gr (String, String, MOnto.OntoObjectType) String -> [Int] -> [Int]
    getNodesToStay  [] isaGraph graph list = list
    getNodesToStay  ((nId,(shPar,shSib,shInRel)):rest) isaGraph graph list =
      let parents  = if shPar
                      then OGraph.getParents nId isaGraph
                      else []
          siblings = if shSib
                      then OGraph.getSiblings nId isaGraph
                      else []
          inRelTo  = if shInRel
                      then OGraph.getInRelationTo nId graph
                      else []
      in getNodesToStay rest isaGraph graph (list ++ [(makeNodeIDInt nId)] ++ parents ++ siblings ++ inRelTo)
  --
    removeNodes :: [Int] -> [Int] -> [Int] -> [Int]
    removeNodes (x:rest) delList list =
      if (elem x delList)
        then removeNodes rest delList list
        else removeNodes rest delList (x:list)
    removeNodes [] _ list = list

--------------------------------------------
-- Get's an DaVinceNodeID and returns a real
-- Int.
--
-- this funktion is shit, I'm sure there is
-- a better way, but I don't know
--------------------------------------------
makeNodeIDInt :: DVTypes.NodeId -> Int
makeNodeIDInt (DVTypes.NodeId (nId)) =
  read nId

--------------------------------------------
-- this launch update of graphattributes.
--------------------------------------------
updateGraphAttributes :: OntoSide -> IO ()
updateGraphAttributes ontoSide = do
  send <- IORef.readIORef (sendIO ontoSide)
  onto <- IORef.readIORef (ontologyIO ontoSide)
  config <- IORef.readIORef (configIO ontoSide)
  Debug.trace ("OntoSide: " ++ (OGraph.updateGraphAttributes onto config)) 
                               (send (OGraph.updateGraphAttributes onto config))
  return ()

--------------------------------------------
-- findNodes
--------------------------------------------
findNodes :: frame -> OntoSide -> IO ()
findNodes f ontoSide = do
  send          <- IORef.readIORef (sendIO ontoSide)
  searchString  <- get (tcFind ontoSide) text
  Debug.trace ("OntoSide: " ++ ("menu(navigation(find(" ++ searchString ++ ",false,false)))")) 
                               (send ("menu(navigation(find(\"" ++ searchString ++ "\",false,false)))\n"))

--------------------------------------------
-- This will create a window, where you can
-- change settings for relations.
-- It is important, that one parameter of
-- this widget is the funktion to update the
-- settings.
-- CODEREFACTORING IS REQUIRED!!!!
--------------------------------------------
changeRelationSettings :: Frame a -> String -> OntoSide -> IO ()
changeRelationSettings f selectedRel ontoSide = do
  onto <- IORef.readIORef (ontologyIO ontoSide)
  config <- IORef.readIORef (configIO ontoSide)
  let OntoConf.Config nodes rels packs = config
  d                  <- dialog     f      [text := "OntoSide: " ++ selectedRel ++ "-relation"]
  p                  <- panel      d      []
  pLabel             <- panel      p      []
  chbLabel           <- checkBox   pLabel [enabled := True]
  btnClose           <- button     p      [text := "Close"]
  choFontS           <- choice     p      [enabled := True]
  choFontF           <- choice     p      [enabled := True]
  choEdgePattern     <- choice     p      [enabled := True]
  choHead            <- choice     p      [enabled := True]

  set chbLabel       [checked := (isShowLabel  (getObject selectedRel rels))
                     ,on command := updGraphAttribAndConfLabel selectedRel chbLabel ontoSide config]
  set choFontS       [items := (uFontStyle ontoSide)
                     ,selection := (getPos (getFontStyle  (getObject selectedRel rels))  (uFontStyle ontoSide)  0)
                     ,on select := updGraphAttribAndConf "fontStyle" selectedRel choFontS ontoSide config]
  set choFontF       [items := (uFontFamily ontoSide)
                     ,selection := (getPos (getFontFamily  (getObject selectedRel rels))  (uFontFamily ontoSide)  0)
                     ,on select := updGraphAttribAndConf "fontFamily" selectedRel choFontF ontoSide config]
  set choEdgePattern [items := (uEdgePattern ontoSide)
                     ,selection := (getPos (getPattern  (getObject selectedRel rels))  (uEdgePattern ontoSide)  0)
                     ,on select := updGraphAttribAndConf "pattern" selectedRel choEdgePattern ontoSide config]
  set choHead        [items := (uEdgeHead ontoSide)
                     ,selection := (getPos (getHead  (getObject selectedRel rels))  (uEdgeHead ontoSide)  0)
                     ,on select := updGraphAttribAndConf "head" selectedRel choHead ontoSide config]

  set d [layout := container p $ margin 10 (grid 5 5
                          [[container pLabel $ row 0 [floatLeft $ widget chbLabel, floatLeft $ label "show label"], empty]
                          ,[floatLeft $ label "Font-Style: ", widget choFontS]
                          ,[floatLeft $ label "Font-Family: ", widget choFontF]
                          ,[floatLeft $ label "Pattern: ", widget choEdgePattern]
                          ,[floatLeft $ label "Head: ", widget choHead]
                          ,[floatRight $ widget btnClose]])]
  ret   <- showModal d $ \stop -> set btnClose [on command := stop Nothing]
  focusOn btnClose
  where
  updGraphAttribAndConf :: (Selection w, Items w String) => String -> String -> w -> OntoSide -> OntoConf.Config -> IO ()
  updGraphAttribAndConf settingType objectType w ontoSide config = do
    i <- get w selection
    s <- get w (item i)
    let OntoConf.Config nodes rels packs = config
    let config2 = case settingType of
                    "fontFamily" -> OntoConf.Config nodes (setFontFamily  objectType rels s) packs
                    "fontStyle"  -> OntoConf.Config nodes (setFontStyle   objectType rels s) packs
                    "pattern"    -> OntoConf.Config nodes (setPattern     objectType rels s) packs
                    "head"       -> OntoConf.Config nodes (setHead        objectType rels s) packs
    IORef.writeIORef (configIO ontoSide) config2
    HaXml.Xml2Haskell.fWriteXml ((pwd ontoSide) ++ "/resource/newontoside.xml") config2
    updateGraphAttributes ontoSide
    return ()
  --
  relations :: MOnto.MMiSSOntology -> [String]
  relations onto =
    map extractRelationName (MOnto.getRelations onto)
    where
    extractRelationName (name, _, _, _) = name
  --
  getPos :: String -> [String] -> Int -> Int
  getPos pattern [] pos = 0                 -- for the impossible case, that there is no match
  getPos pattern (x:rest) pos = if pattern == x then pos else getPos pattern rest (pos+1)
  --
  isShowLabel :: OntoConf.Relation -> Bool
  isShowLabel object = if (OntoConf.relationShowLabel object) == "true" then True else False
  --
  getFontStyle :: OntoConf.Relation -> String
  getFontStyle object = (OntoConf.relationFontStyle object)
  --
  getFontFamily :: OntoConf.Relation -> String
  getFontFamily object = (OntoConf.relationFontFamily object)
  --
  getPattern :: OntoConf.Relation -> String
  getPattern object = (OntoConf.relationEdgePattern object)
  --
  getHead :: OntoConf.Relation -> String
  getHead object = (OntoConf.relationHead object)
  --
  getObject :: String -> OntoConf.Relations -> OntoConf.Relation
  getObject searchString (OntoConf.Relations objects) =
    foldl1 isObject objects
    where
    isObject x y
        | (OntoConf.relationLabel x) == searchString = x
        | otherwise = y
  --
  setShowLabel :: String -> OntoConf.Relations -> Bool -> OntoConf.Relations
  setShowLabel searchString (OntoConf.Relations objects) newValue = (OntoConf.Relations (map (update searchString newValue) objects))
    where
    update searchString newValue object = if (OntoConf.relationLabel object) == searchString
                                          then OntoConf.Relation { OntoConf.relationShowLabel   = if newValue then "true" else "false",
                                                                   OntoConf.relationLabel       = (OntoConf.relationLabel object),
                                                                   OntoConf.relationFontStyle   = (OntoConf.relationFontStyle object),
                                                                   OntoConf.relationFontFamily  = (OntoConf.relationFontFamily object),
                                                                   OntoConf.relationColor       = (OntoConf.relationColor object),
                                                                   OntoConf.relationEdgePattern = (OntoConf.relationEdgePattern object),
                                                                   OntoConf.relationHead        = (OntoConf.relationHead object)}
                                          else object
  setFontStyle :: String -> OntoConf.Relations -> String -> OntoConf.Relations
  setFontStyle searchString (OntoConf.Relations objects) newValue = (OntoConf.Relations (map (update searchString newValue) objects))
    where
    update searchString newValue object = if (OntoConf.relationLabel object) == searchString
                                          then OntoConf.Relation { OntoConf.relationShowLabel   = (OntoConf.relationShowLabel object),
                                                                   OntoConf.relationLabel       = (OntoConf.relationLabel object),
                                                                   OntoConf.relationFontStyle   = newValue,
                                                                   OntoConf.relationFontFamily  = (OntoConf.relationFontFamily object),
                                                                   OntoConf.relationColor       = (OntoConf.relationColor object),
                                                                   OntoConf.relationEdgePattern = (OntoConf.relationEdgePattern object),
                                                                   OntoConf.relationHead        = (OntoConf.relationHead object)}
                                          else object
  setFontFamily :: String -> OntoConf.Relations -> String -> OntoConf.Relations
  setFontFamily searchString (OntoConf.Relations objects) newValue = (OntoConf.Relations (map (update searchString newValue) objects))
    where
    update searchString newValue object = if (OntoConf.relationLabel object) == searchString
                                          then OntoConf.Relation { OntoConf.relationShowLabel   = (OntoConf.relationShowLabel object),
                                                                   OntoConf.relationLabel       = (OntoConf.relationLabel object),
                                                                   OntoConf.relationFontStyle   = (OntoConf.relationFontStyle object),
                                                                   OntoConf.relationFontFamily  = newValue,
                                                                   OntoConf.relationColor       = (OntoConf.relationColor object),
                                                                   OntoConf.relationEdgePattern = (OntoConf.relationEdgePattern object),
                                                                   OntoConf.relationHead        = (OntoConf.relationHead object)}
                                          else object
  setPattern :: String -> OntoConf.Relations -> String -> OntoConf.Relations
  setPattern searchString (OntoConf.Relations objects) newValue = (OntoConf.Relations (map (update searchString newValue) objects))
    where
    update searchString newValue object = if (OntoConf.relationLabel object) == searchString
                                          then OntoConf.Relation { OntoConf.relationShowLabel   = (OntoConf.relationShowLabel object),
                                                                   OntoConf.relationLabel       = (OntoConf.relationLabel object),
                                                                   OntoConf.relationFontStyle   = (OntoConf.relationFontStyle object),
                                                                   OntoConf.relationFontFamily  = (OntoConf.relationFontFamily object),
                                                                   OntoConf.relationColor       = (OntoConf.relationColor object),
                                                                   OntoConf.relationEdgePattern = newValue,
                                                                   OntoConf.relationHead        = (OntoConf.relationHead object)}
                                          else object

  setHead :: String -> OntoConf.Relations -> String -> OntoConf.Relations
  setHead searchString (OntoConf.Relations objects) newValue = (OntoConf.Relations (map (update searchString newValue) objects))
    where
    update searchString newValue object = if (OntoConf.relationLabel object) == searchString
                                          then OntoConf.Relation { OntoConf.relationShowLabel   = (OntoConf.relationShowLabel object),
                                                                   OntoConf.relationLabel       = (OntoConf.relationLabel object),
                                                                   OntoConf.relationFontStyle   = (OntoConf.relationFontStyle object),
                                                                   OntoConf.relationFontFamily  = (OntoConf.relationFontFamily object),
                                                                   OntoConf.relationColor       = (OntoConf.relationColor object),
                                                                   OntoConf.relationEdgePattern = (OntoConf.relationEdgePattern object),
                                                                   OntoConf.relationHead        = newValue}
                                          else object
  updGraphAttribAndConfLabel :: (Checkable w) => String -> w -> OntoSide -> OntoConf.Config -> IO ()
  updGraphAttribAndConfLabel objectType w ontoSide config = do
    showLabel <- get w checked
    let OntoConf.Config nodes rels packs = config
    let config2 = OntoConf.Config nodes (setShowLabel objectType rels showLabel) packs
    IORef.writeIORef (configIO ontoSide) config2
    HaXml.Xml2Haskell.fWriteXml ((pwd ontoSide) ++ "/resource/newontoside.xml") config2
    updateGraphAttributes ontoSide
    return ()
    where
    setShowLabel :: String -> OntoConf.Relations -> Bool -> OntoConf.Relations
    setShowLabel searchString (OntoConf.Relations objects) newValue = (OntoConf.Relations (map (update searchString newValue) objects))
      where
      update searchString newValue object = if (OntoConf.relationLabel object) == searchString
                                             then OntoConf.Relation { OntoConf.relationShowLabel   = if newValue then "true" else "false",
                                                                      OntoConf.relationLabel       = (OntoConf.relationLabel object),
                                                                      OntoConf.relationFontStyle   = (OntoConf.relationFontStyle object),
                                                                      OntoConf.relationFontFamily  = (OntoConf.relationFontFamily object),
                                                                      OntoConf.relationColor       = (OntoConf.relationColor object),
                                                                      OntoConf.relationEdgePattern = (OntoConf.relationEdgePattern object),
                                                                      OntoConf.relationHead        = (OntoConf.relationHead object)}
                                             else object