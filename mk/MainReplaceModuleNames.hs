import qualified Data.Map as Map
import Data.Char
import Data.List
import System.Environment

data State = Other | Identifier deriving Eq

-- Identifier fangen mit Buchstaben oder "_" an.
-- Ziffern sind nach dem ersten Buchstaben legal.
newState :: Char -> State -> State
newState c st = case st of
  Identifier -> if isAlphaNum c || elem c "_." then Identifier else Other
  _ -> if isAlpha c || c == '_' then Identifier else Other

tokenize :: State -> String -> [String]
tokenize st input = case input of
   "" -> [""]   -- Anfangswort (wird ggf. durch concat ignoriert)
   c : rest -> let
       nst = newState c st
       w : text = tokenize nst rest
       in if st == nst
          then (c : w) : text       -- weiter sammeln
          else "" : (c : w) : text  -- neues Wort anfangen

process :: Map.Map String String -> String -> String
process m str = unlines $
  map (\ l -> case tokenize Other l of
         "" : "import" : _ : "qualified" : _ : modname : _ : "as" : r ->
             case Map.lookup modname m of
               Nothing -> l
               Just qv -> "import qualified " ++ qv ++ " as" ++ concat r
         "" : "import" : _ : "qualified" : _ : modname : r ->
             case Map.lookup modname m of
               Nothing -> l
               Just qv ->
                 "import qualified " ++ qv ++ " as " ++ modname ++ concat r
         "" : "import" : _ : modname : r ->
             case Map.lookup modname m of
               Nothing -> l
               Just qv -> "import " ++ qv ++ concat r
         bs : "module" : _ : modname : r ->
             case Map.lookup modname m of
               Nothing -> l
               Just qv -> bs ++ "module " ++ qv ++ concat r
         _ -> l -- concatMap (\ s -> Map.findWithDefault s s m) ts
      ) $ lines str

processM :: String -> IO ()
processM file = do
  str <- readFile file
  putStrLn $ "processing: " ++ file ++ " (" ++ show (length str) ++ " chars)"
  writeFile file $ process transMap str

main :: IO ()
main = getArgs >>= mapM_ processM

transMap :: Map.Map String String
transMap = Map.fromList $ map ( \ str ->
  case splitOn '.' str of
    s@(f : _) -> let l = last s in
       if f == "MMiSS" && isPrefixOf "MMiSS" l then
           (l, intercalate "." $ init s ++ [drop 5 l])
       else if f == "UDrawGraph" && isPrefixOf "DaVinci" l then
           (l, intercalate "." $ init s ++ [drop 7 l])
       else (l, str)
    _ -> error "transMap") newmodules

splitOn :: Eq a => a -- ^ seperator
        -> [a] -- ^ list to split
        -> [[a]]
splitOn x xs = let (l, r) = break (==x) xs in
    (if null l then [] else [l]) ++ (if null r then [] else splitOn x $ tail r)

newmodules :: [String]
newmodules =
 [ "Appl.Ontologytool.AbstractGraphView"
 , "Appl.Ontologytool.MMiSSOntology"
 , "Appl.Ontologytool.MMiSSOntologyGraph"
 , "Appl.Ontologytool.OntoParser"
 , "Appl.Ontologytool.OntoTool"
 , "Emacs.EmacsBasic"
 , "Emacs.EmacsCommands"
 , "Emacs.EmacsContent"
 , "Emacs.EmacsEdit"
 , "Emacs.EmacsSExp"
 , "Emacs.Extents"
 , "Events.After"
 , "Events.Cells"
 , "Events.Channels"
 , "Events.DeleteQueue"
 , "Events.Destructible"
 , "Events.EqGuard"
 , "Events.Events"
 , "Events.Examples"
 , "Events.FMQueue"
 , "Events.GuardBasics"
 , "Events.GuardedChannels"
 , "Events.GuardedEvents"
 , "Events.MatchChannel"
 , "Events.NullGuard"
 , "Events.RefQueue"
 , "Events.RegexChannel"
 , "Events.Spawn"
 , "Events.Synchronized"
 , "Events.Toggle"
 , "Events.WrapIO"
 , "Graphs.DisplayGraph"
 , "Graphs.EmptyGraphSort"
 , "Graphs.FindCommonParents"
 , "Graphs.FindCycle"
 , "Graphs.GetAncestors"
 , "Graphs.GetAttributes"
 , "Graphs.Graph"
 , "Graphs.GraphConfigure"
 , "Graphs.GraphConnection"
 , "Graphs.GraphDisp"
 , "Graphs.GraphEditor"
 , "Graphs.GraphEditorRemote"
 , "Graphs.GraphEditorService"
 , "Graphs.GraphOps"
 , "Graphs.NewNames"
 , "Graphs.PureGraph"
 , "Graphs.PureGraphMakeConsistent"
 , "Graphs.PureGraphPrune"
 , "Graphs.PureGraphToGraph"
 , "Graphs.RemoveAncestors"
 , "Graphs.SimpleGraph"
 , "Graphs.TopSort"
 , "Graphs.VersionDag"
 , "Htk.Canvasitems.Arc"
 , "Htk.Canvasitems.BitMapItem"
 , "Htk.Canvasitems.CanvasItem"
 , "Htk.Canvasitems.CanvasItemAux"
 , "Htk.Canvasitems.CanvasTag"
 , "Htk.Canvasitems.EmbeddedCanvasWin"
 , "Htk.Canvasitems.ImageItem"
 , "Htk.Canvasitems.Line"
 , "Htk.Canvasitems.Oval"
 , "Htk.Canvasitems.Polygon"
 , "Htk.Canvasitems.Rectangle"
 , "Htk.Canvasitems.TextItem"
 , "Htk.Components.BitMap"
 , "Htk.Components.Focus"
 , "Htk.Components.ICursor"
 , "Htk.Components.Icon"
 , "Htk.Components.Image"
 , "Htk.Components.Index"
 , "Htk.Components.Selection"
 , "Htk.Components.Slider"
 , "Htk.Containers.Box"
 , "Htk.Containers.Frame"
 , "Htk.Containers.Toplevel"
 , "Htk.Containers.Window"
 , "Htk.Devices.Bell"
 , "Htk.Devices.Printer"
 , "Htk.Devices.Screen"
 , "Htk.Devices.XSelection"
 , "Htk.Kernel.BaseClasses"
 , "Htk.Kernel.ButtonWidget"
 , "Htk.Kernel.CallWish"
 , "Htk.Kernel.Colour"
 , "Htk.Kernel.Configuration"
 , "Htk.Kernel.Core"
 , "Htk.Kernel.Cursor"
 , "Htk.Kernel.EventInfo"
 , "Htk.Kernel.Font"
 , "Htk.Kernel.GUIObject"
 , "Htk.Kernel.GUIObjectKind"
 , "Htk.Kernel.GUIObjectName"
 , "Htk.Kernel.GUIValue"
 , "Htk.Kernel.Geometry"
 , "Htk.Kernel.GridPackOptions"
 , "Htk.Kernel.PackOptions"
 , "Htk.Kernel.Packer"
 , "Htk.Kernel.Resources"
 , "Htk.Kernel.TkVariables"
 , "Htk.Kernel.Tooltip"
 , "Htk.Kernel.Wish"
 , "Htk.Menuitems.Indicator"
 , "Htk.Menuitems.Menu"
 , "Htk.Menuitems.MenuCascade"
 , "Htk.Menuitems.MenuCheckButton"
 , "Htk.Menuitems.MenuCommand"
 , "Htk.Menuitems.MenuItem"
 , "Htk.Menuitems.MenuRadioButton"
 , "Htk.Menuitems.MenuSeparator"
 , "Htk.Textitems.EmbeddedTextWin"
 , "Htk.Textitems.Mark"
 , "Htk.Textitems.TextTag"
 , "Htk.Tix.ComboBox"
 , "Htk.Tix.LabelFrame"
 , "Htk.Tix.NoteBook"
 , "Htk.Tix.PanedWindow"
 , "Htk.Tix.Subwidget"
 , "Htk.Toolkit.CItem"
 , "Htk.Toolkit.DialogWin"
 , "Htk.Toolkit.DragAndDrop"
 , "Htk.Toolkit.FileDialog"
 , "Htk.Toolkit.GenGUI"
 , "Htk.Toolkit.GenericBrowser"
 , "Htk.Toolkit.HTkMenu"
 , "Htk.Toolkit.IconBar"
 , "Htk.Toolkit.InputForm"
 , "Htk.Toolkit.InputWin"
 , "Htk.Toolkit.LogWin"
 , "Htk.Toolkit.MarkupText"
 , "Htk.Toolkit.MenuType"
 , "Htk.Toolkit.ModalDialog"
 , "Htk.Toolkit.Name"
 , "Htk.Toolkit.Notepad"
 , "Htk.Toolkit.Prompt"
 , "Htk.Toolkit.ScrollBox"
 , "Htk.Toolkit.SelectBox"
 , "Htk.Toolkit.Separator"
 , "Htk.Toolkit.SimpleForm"
 , "Htk.Toolkit.SimpleListBox"
 , "Htk.Toolkit.SpinButton"
 , "Htk.Toolkit.TextDisplay"
 , "Htk.Toolkit.TreeList"
 , "Htk.Toplevel.HTk"
 , "Htk.Widgets.Button"
 , "Htk.Widgets.Canvas"
 , "Htk.Widgets.CheckButton"
 , "Htk.Widgets.ComboBox"
 , "Htk.Widgets.Editor"
 , "Htk.Widgets.Entry"
 , "Htk.Widgets.Label"
 , "Htk.Widgets.ListBox"
 , "Htk.Widgets.MenuButton"
 , "Htk.Widgets.Message"
 , "Htk.Widgets.OptionMenu"
 , "Htk.Widgets.RadioButton"
 , "Htk.Widgets.Scale"
 , "Htk.Widgets.ScrollBar"
 , "Htk.Widgets.Space"
 , "Imports.Aliases"
 , "Imports.EntityNames"
 , "Imports.Environment"
 , "Imports.ErrorManagement"
 , "Imports.ErrorReporting"
 , "Imports.FolderStructure"
 , "Imports.Imports"
 , "MMiSS.Api.MMiSSAPI"
 , "MMiSS.Api.MMiSSAPIBasics"
 , "MMiSS.Api.MMiSSAPIBlock"
 , "MMiSS.Api.MMiSSCallServer"
 , "MMiSS.Api.MMiSSCheckOutCommit"
 , "MMiSS.Api.MMiSSDoXml"
 , "MMiSS.Api.MMiSSGetPut"
 , "MMiSS.Api.MMiSSMapVersionInfo"
 , "MMiSS.Api.MMiSSMessages"
 , "MMiSS.Api.MMiSSRequest"
 , "MMiSS.Api.MMiSSSecurityOps"
 , "MMiSS.Api.MMiSSSessionState"
 , "MMiSS.Api.MMiSSToFromBundle"
 , "MMiSS.MMiSSActiveMath"
 , "MMiSS.MMiSSAttributes"
 , "MMiSS.MMiSSBundle"
 , "MMiSS.MMiSSBundleConvert"
 , "MMiSS.MMiSSBundleDissect"
 , "MMiSS.MMiSSBundleFillIn"
 , "MMiSS.MMiSSBundleNodeCheckTypes"
 , "MMiSS.MMiSSBundleNodeEditLocks"
 , "MMiSS.MMiSSBundleNodeWrite"
 , "MMiSS.MMiSSBundleNodeWriteClass"
 , "MMiSS.MMiSSBundleNodeWriteObject"
 , "MMiSS.MMiSSBundleReadFiles"
 , "MMiSS.MMiSSBundleSimpleUtils"
 , "MMiSS.MMiSSBundleTypes"
 , "MMiSS.MMiSSBundleUtils"
 , "MMiSS.MMiSSBundleValidate"
 , "MMiSS.MMiSSBundleWrite"
 , "MMiSS.MMiSSCheck"
 , "MMiSS.MMiSSCheck_DTD"
 , "MMiSS.MMiSSDTD"
 , "MMiSS.MMiSSDTDAssumptions"
 , "MMiSS.MMiSSDisplay"
 , "MMiSS.MMiSSEditAttributes"
 , "MMiSS.MMiSSEditFormatConverter"
 , "MMiSS.MMiSSEditLocks"
 , "MMiSS.MMiSSEditXml"
 , "MMiSS.MMiSSElementInfo"
 , "MMiSS.MMiSSElementInstances"
 , "MMiSS.MMiSSEmacsEdit"
 , "MMiSS.MMiSSExportEntireBundle"
 , "MMiSS.MMiSSExportFiles"
 , "MMiSS.MMiSSExportLaTeX"
 , "MMiSS.MMiSSExportVariantBundle"
 , "MMiSS.MMiSSFileSystemExamples"
 , "MMiSS.MMiSSFileType"
 , "MMiSS.MMiSSFiles"
 , "MMiSS.MMiSSFormat"
 , "MMiSS.MMiSSGetVariantAttributes"
 , "MMiSS.MMiSSImportExportBundle"
 , "MMiSS.MMiSSImportExportErrors"
 , "MMiSS.MMiSSInitialise"
 , "MMiSS.MMiSSInsertionPoint"
 , "MMiSS.MMiSSLaTeX"
 , "MMiSS.MMiSSObjectExtract"
 , "MMiSS.MMiSSObjectType"
 , "MMiSS.MMiSSObjectTypeInstance"
 , "MMiSS.MMiSSObjectTypeType"
 , "MMiSS.MMiSSOntologyParser"
 , "MMiSS.MMiSSOntologyStore"
 , "MMiSS.MMiSSPackageFolder"
 , "MMiSS.MMiSSPreamble"
 , "MMiSS.MMiSSPrint"
 , "MMiSS.MMiSSReAssemble"
 , "MMiSS.MMiSSReadObject"
 , "MMiSS.MMiSSRegistrations"
 , "MMiSS.MMiSSRunCommand"
 , "MMiSS.MMiSSSplitLink"
 , "MMiSS.MMiSSSubFolder"
 , "MMiSS.MMiSSUpdateVariantObject"
 , "MMiSS.MMiSSVariant"
 , "MMiSS.MMiSSVariantObject"
 , "MMiSS.MMiSSXmlBasics"
 , "MMiSS.Parser.LaTeXParser"
 , "MMiSS.Parser.LaTeXParserCore"
 , "MMiSS.Parser.LaTeXPreamble"
 , "MMiSS.Parser.OntoParser"
 , "Posixutil.BlockSigPIPE"
 , "Posixutil.ChildProcess"
 , "Posixutil.CopyFile"
 , "Posixutil.Expect"
 , "Posixutil.FdRead"
 , "Posixutil.ProcessClasses"
 , "Posixutil.SafeSystem"
 , "Reactor.BSem"
 , "Reactor.InfoBus"
 , "Reactor.Lock"
 , "Reactor.LockEvent"
 , "Reactor.MSem"
 , "Reactor.ReferenceVariables"
 , "Reactor.WithDir"
 , "Server.CallServer"
 , "Server.Crypt"
 , "Server.EchoService"
 , "Server.GroupFile"
 , "Server.Hosts"
 , "Server.HostsList"
 , "Server.HostsPorts"
 , "Server.LogFile"
 , "Server.MultiPlexer"
 , "Server.MultiServer"
 , "Server.Notification"
 , "Server.PasswordFile"
 , "Server.Server"
 , "Server.ServiceClass"
 , "Server.ZLib"
 , "Simpledb.BDBExtras"
 , "Simpledb.BDBOps"
 , "Simpledb.Commit"
 , "Simpledb.ExaminePermissions"
 , "Simpledb.FlushSimpleDB"
 , "Simpledb.GetDiffs"
 , "Simpledb.LastChange"
 , "Simpledb.LocationAllocation"
 , "Simpledb.ModifyUserInfo"
 , "Simpledb.ObjectSource"
 , "Simpledb.ObjectVersion"
 , "Simpledb.OpenSimpleDB"
 , "Simpledb.Permissions"
 , "Simpledb.PrimitiveLocation"
 , "Simpledb.QuerySimpleDB"
 , "Simpledb.Retrieve"
 , "Simpledb.SecurityManagement"
 , "Simpledb.ServerErrors"
 , "Simpledb.SetGetSecurityData"
 , "Simpledb.SimpleDB"
 , "Simpledb.SimpleDBServer"
 , "Simpledb.SimpleDBService"
 , "Simpledb.SimpleDBTypes"
 , "Simpledb.VersionAllocation"
 , "Simpledb.VersionData"
 , "Simpledb.VersionInfo"
 , "Simpledb.VersionInfoFilter"
 , "Simpledb.VersionInfoService"
 , "Simpledb.VersionState"
 , "Types.AttributesType"
 , "Types.BasicObjects"
 , "Types.CallEditor"
 , "Types.CodedValue"
 , "Types.CodedValueType"
 , "Types.CopyVersion"
 , "Types.CopyVersionInfos"
 , "Types.CopyVersions"
 , "Types.CreateObjectMenu"
 , "Types.DisplayParms"
 , "Types.DisplayTypes"
 , "Types.DisplayView"
 , "Types.Files"
 , "Types.Folders"
 , "Types.GetAttributesType"
 , "Types.GlobalMenus"
 , "Types.GlobalRegistry"
 , "Types.Initialisation"
 , "Types.Link"
 , "Types.LinkDrawer"
 , "Types.LinkManager"
 , "Types.LocalMenus"
 , "Types.ManagePermissions"
 , "Types.MergeComputeParents"
 , "Types.MergePrune"
 , "Types.MergeReAssign"
 , "Types.MergeTypes"
 , "Types.Merging"
 , "Types.NoAccessObject"
 , "Types.ObjectTypes"
 , "Types.Registrations"
 , "Types.SpecialNodeActions"
 , "Types.ToggleAdminStatus"
 , "Types.VersionDB"
 , "Types.VersionGraph"
 , "Types.VersionGraphClient"
 , "Types.VersionGraphList"
 , "Types.View"
 , "Types.ViewType"
 , "UDrawGraph.DaVinciBasic"
 , "UDrawGraph.DaVinciGraph"
 , "UDrawGraph.DaVinciTypes"
 , "Util.AtomString"
 , "Util.Binary"
 , "Util.BinaryAll"
 , "Util.BinaryExtras"
 , "Util.BinaryInstances"
 , "Util.BinaryUtils"
 , "Util.Broadcaster"
 , "Util.Bytes"
 , "Util.Cache"
 , "Util.CacheTable"
 , "Util.ClockTimeToString"
 , "Util.CommandStringSub"
 , "Util.CompileFlags"
 , "Util.Computation"
 , "Util.Debug"
 , "Util.DeepSeq"
 , "Util.DelSet"
 , "Util.Delayer"
 , "Util.DeprecatedFiniteMap"
 , "Util.DeprecatedSet"
 , "Util.Dynamics"
 , "Util.ExtendedPrelude"
 , "Util.FileNames"
 , "Util.FileSystem"
 , "Util.HostName"
 , "Util.Huffman"
 , "Util.ICStringLen"
 , "Util.IOExtras"
 , "Util.IntPlus"
 , "Util.KeyedChanges"
 , "Util.LineShow"
 , "Util.Maybes"
 , "Util.Messages"
 , "Util.Myers"
 , "Util.NameMangle"
 , "Util.Object"
 , "Util.Queue"
 , "Util.QuickReadShow"
 , "Util.ReferenceCount"
 , "Util.Registry"
 , "Util.RegularExpression"
 , "Util.Sink"
 , "Util.SmallSet"
 , "Util.Sources"
 , "Util.Store"
 , "Util.StrError"
 , "Util.StringSkip"
 , "Util.TSem"
 , "Util.TempFile"
 , "Util.TemplateHaskellHelps"
 , "Util.Thread"
 , "Util.ThreadDict"
 , "Util.UTF8"
 , "Util.UnionFind"
 , "Util.UniqueFile"
 , "Util.UniqueString"
 , "Util.VSem"
 , "Util.VariableList"
 , "Util.VariableMap"
 , "Util.VariableSet"
 , "Util.VariableSetBlocker"
 , "Util.VisitedSet"
 , "Util.WBFiles"
 , "Util.WaitOnN" ]
