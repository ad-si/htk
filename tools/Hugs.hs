{- #########################################################################

MODULE        : Hugs
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : An encapsulation of Hugs as a workbench tool!

TO BE DONE    : Underlines, accelerators and key bindings!

                Get rid of stderr output (filter \b etc)
                Fonts
                Filechooser

                And, the Tk text widget has sincere problems digesting
                huge one liners as the one that hugs generate in
                response to e.g. [1..10000]. String should probably
                be split into lines of max N chars, with N equal to some
                magic number!


   ######################################################################### -}


module Hugs (
        Object(..),
        Tool(..),

        Toggle(..),

        Hugs,
        FilePath,
        HugsSourceObj(..),

        getImports,

        Config,
        printStatistics,
        printTypeAfterEval,
        terminateOnError,
        gcNotification,
        literateModules,
        listFilesLoaded,
        displayDotsWhileLoading,
        useShowToDisplayResults,
        detailedKindErrors,
        importChasing,
        setPrompt,
        getPrompt,
        setRepeatString,
        getRepeatString,

        interruptHugs,
        shellEscape,
        changeModule,
        changeDirectory,
        forceGC,
        exitHugs,
        loadDefinitions,
        loadAdditionalFiles,
        repeatLastLoad,
        loadProject,
        editLoadedFile,
        editLastFile,
        findDefinition,
        listNames,
        listCommands,
        printTypeOfExpr,
        displayInfoAboutNames,

        ) where

import WB
import EventLoop
import ScrollBox
import Editor
import Keyboard
import Frame
import PulldownMenu
import MenuItem
import Separator
import Button
import CheckButton
import Space
import PromptWin
import Bell
import DialogWin

import Posix (Signal,signalProcess,sigINT)
import ChildProcess(standarderrors)
import WBFiles


import Expect
import Debug(debug)

import EventStream
import Variable
import HTk
import Thread
-- --------------------------------------------------------------------------
-- Semantic Domains
-- --------------------------------------------------------------------------

data Hugs = Hugs 
                Expect                          -- background intrp
                Window                          -- main window
                (Editor String)                 -- text widget
                (EventStream ())                -- event dispatcher
                (PVar (String,String))          -- prompt, repeat string  ...



-- --------------------------------------------------------------------------
-- Classes
-- --------------------------------------------------------------------------

class HugsSourceObj o where
        newHugs :: o -> [Config PosixProcess] -> IO Hugs

                
-- --------------------------------------------------------------------------
--  Tool Instance
-- --------------------------------------------------------------------------

instance Object Hugs where
        objectID (Hugs tool _ _ _ _) = objectID tool


instance Destructible Hugs where
        destroy (Hugs tool win _ _ _) =  do {
                destroy tool;
                try (destroy win);
                done
                }
        destroyed (Hugs tool _ _ _ _) = destroyed tool

instance Tool Hugs where
        getToolStatus (Hugs tool _ _ _ _) = getToolStatus tool

                
-- --------------------------------------------------------------------------
--  Commands
-- --------------------------------------------------------------------------

instance HugsSourceObj [FilePath] where
        newHugs files confs = do {      
                exp <- newExpect "hugs" (confs++[standarderrors False, args]);
                state <- newPVar ("Prelude>","$$");
                es <- newEventStream;

                (win,tp,bm) <- newHugsWindow exp;
                hugs <- return (Hugs exp win tp es state);
                newHugsMenu hugs tp bm;

                matchUntilPrompt exp win tp;

                when (files /= []) (hugs # loadDefinitions files);

                interactor (\iact ->
                        receive es  |>> done
                 +>     matchPrompt exp tp "Prelude> " >>> done
                 +>     expect exp ("^.*\n",2::Int) >>>= appendText tp
                 +>     expect exp (".+", 1::Int) >>>= appendText tp 
                 +>     match exp ("\n",0::Int) >>> insertNewline tp
                 +>     terminated exp win >>> do {
                                become es (inaction::IA ()); stop iact}                 
                 +>     keyPressed tp "Return" >>> do {
                                cmd <- getCommand tp;
                                execCmd (cmd ++ "\n") exp
                                }
                 );
                return hugs
        } where args = arguments (
                        ["-Etextedit %s","-s","-t","-f","-g","-w","-.","-u","-k","-i"])
                matchPrompt exp tp pr = 
                        expect exp (('^':stringToPtn pr), 4::Int) >>>= 
                                appendText tp
                stringToPtn [] = []
                stringToPtn ('?':str) = "\\?" ++ stringToPtn str
                stringToPtn (c:str) = c : stringToPtn str

                
-- --------------------------------------------------------------------------
--  Utils
-- --------------------------------------------------------------------------

terminated exp win = 
        destroyed win >>> destroy exp
   +>   destroyed exp >>> destroy win
   +>   matchEOF exp >>> destroy win

getCommand :: Editor String -> IO String 
getCommand tp = do 
        str <- getTextLine tp (EndOfText,BackwardLines 2)
        case dropWhile (/= '>') str of
                []    -> return []
                (x:l) -> return l




matchUntilPrompt :: Expect -> Window -> Editor String -> IO Bool
matchUntilPrompt exp win tp  = do 
   es <- newEventStream
   become es (
           matchLine exp     >>>= (\str -> do {
                (case str of 
                   'P':'a':'r':'s':'i':_ -> done
                   _ -> appendText tp str); 
                return True
                })
        +> expect exp "^Prelude> " >>>= (\str -> do {
                appendText tp str;
                become es (inaction :: IA Bool); 
                return False
                })
        +> terminated exp win >>> do {
                become es (inaction :: IA Bool); 
                raise hugsFailed
                }
       )
   while (receiveIO es) id

hugsFailed = toolFailed "hugs"

                
-- --------------------------------------------------------------------------
--  Hugs Import Chaser
-- --------------------------------------------------------------------------

getImports :: FilePath -> IO [String]
getImports fname = do { 
        exp <- newExpect "grep" [
                        arguments ["import",fname],
                        pollinterval (Just (secs 0.1))
                        ];
        ch <- newChannel;
        interactor (chaser exp ch []);
        sync (receive ch);
} where chaser exp ch imports iact = 
                matchLine exp >>> done 
           +>   expect exp ("^import .*\n",1::Int) >>>= (\str ->
                        become iact (chaser exp ch ((fetchImports str) ++ imports) iact))
           +>   matchEOF exp >>> do {sendIO ch (reverse imports); stop iact}
        fetchImports str = words (takeWhile notEnd (drop 6 str))
        notEnd '(' = False
        notEnd ';' = False
        notEnd _ = True


                
-- --------------------------------------------------------------------------
--  Hugs Configure Options
-- --------------------------------------------------------------------------

printStatistics :: Toggle -> Config Hugs
printStatistics t hugs = configureHugs (":set " ++ option t ++ "s") hugs

printTypeAfterEval :: Toggle -> Config Hugs
printTypeAfterEval t hugs = configureHugs (":set " ++ option t ++ "t") hugs

terminateOnError :: Toggle -> Config Hugs
terminateOnError t hugs = configureHugs (":set " ++ option t ++ "f") hugs

gcNotification :: Toggle -> Config Hugs
gcNotification t hugs = configureHugs (":set " ++ option t ++ "g") hugs 

literateModules :: Toggle -> Config Hugs
literateModules t hugs = configureHugs (":set " ++ option t ++ "l") hugs

listFilesLoaded :: Toggle -> Config Hugs
listFilesLoaded t hugs = configureHugs (":set " ++ option t ++ "w") hugs

displayDotsWhileLoading :: Toggle -> Config Hugs
displayDotsWhileLoading t hugs = configureHugs (":set " ++ option t ++ ".") hugs

useShowToDisplayResults :: Toggle -> Config Hugs
useShowToDisplayResults t hugs = configureHugs (":set " ++ option t ++ "u") hugs

detailedKindErrors :: Toggle -> Config Hugs
detailedKindErrors t hugs = configureHugs (":set " ++ option t ++ "k") hugs

importChasing :: Toggle -> Config Hugs
importChasing t hugs = configureHugs (":set " ++ option t ++ "i") hugs

configureHugs c h = do {execHugsCmd c h; return h}

option On = "+"
option Off = "-"
        
        
-- --------------------------------------------------------------------------
--  Hugs Commands
-- --------------------------------------------------------------------------

shellEscape :: String -> Hugs -> IO ()
shellEscape cmd hugs = execHugsCmd (":!" ++ cmd ++ "") hugs

changeModule :: String -> Hugs -> IO ()
changeModule name hugs = execHugsCmd (":module " ++ name) hugs

changeDirectory :: String -> Hugs -> IO ()
changeDirectory dir hugs = execHugsCmd (":cd " ++ dir) hugs

forceGC :: Hugs -> IO ()
forceGC hugs = execHugsCmd ":gc" hugs

exitHugs :: Hugs -> IO ()
exitHugs hugs = execHugsCmd ":quit" hugs

loadDefinitions :: [String] -> Hugs -> IO ()
loadDefinitions fnms hugs = execHugsCmd (":load " ++ fnms') hugs
        where fnms' = foldr (\f l -> f ++ " " ++ l) [] fnms

loadAdditionalFiles :: String -> Hugs -> IO ()
loadAdditionalFiles fnms hugs = execHugsCmd (":also " ++ fnms) hugs

repeatLastLoad :: Hugs -> IO ()
repeatLastLoad hugs = execHugsCmd ":reload" hugs

loadProject :: FilePath -> Hugs -> IO ()
loadProject fnm hugs = execHugsCmd (":project " ++ fnm) hugs

editLoadedFile :: FilePath -> Hugs -> IO ()
editLoadedFile fnm hugs = execHugsCmd (":edit " ++ fnm) hugs

editLastFile :: Hugs -> IO ()
editLastFile hugs = execHugsCmd ":edit" hugs

findDefinition :: FilePath -> Hugs -> IO ()
findDefinition name hugs = execHugsCmd (":find " ++ name) hugs

listNames :: String -> Hugs -> IO ()
listNames ptn hugs = execHugsCmd (":names " ++ ptn) hugs

listCommands :: Hugs -> IO ()
listCommands hugs = execHugsCmd ":?" hugs

listOptions :: Hugs -> IO ()
listOptions hugs = execHugsCmd ":set" hugs

printTypeOfExpr :: String -> Hugs -> IO ()
printTypeOfExpr expr hugs = execHugsCmd (":type " ++ expr) hugs

displayInfoAboutNames :: String -> Hugs -> IO ()
displayInfoAboutNames names hugs = execHugsCmd (":info " ++ names) hugs

setPrompt :: String -> Hugs -> IO ()
setPrompt t hugs @ (Hugs _ _ _ _ state) = do {
        (_,rs) <- getVar state;
        setVar state (t,rs);
        execHugsCmd (":set +p" ++ t ++ " ") hugs
}

getPrompt :: Hugs -> IO String
getPrompt (Hugs _ _ _ _ state) = do {
        (pr,_) <- getVar state;
        return pr
}

setRepeatString :: String -> Hugs -> IO ()
setRepeatString t hugs @ (Hugs _ _ _ _ state) = do {
        (pr,_) <- getVar state;
        setVar state (pr,t);
        execHugsCmd (":set +r" ++ t) hugs
}

getRepeatString :: Hugs -> IO String
getRepeatString (Hugs _ _ _ _ state) = do {
        (_,rs) <- getVar state;
        return rs
}

interruptHugs :: Hugs -> IO ()
interruptHugs (Hugs exp _ tp _ _) = do {
        insertNewline tp;
        pid <- getUnixProcessID exp;
        signalProcess sigINT pid;
        done
}


execHugsCmd cmd (Hugs exp _ tp _ _) = do
        insertText tp EndOfText cmd
        insertNewline tp
        execOneWayCmd (cmd ++ "\n") exp


-- --------------------------------------------------------------------------
--  Extended Command Set
-- --------------------------------------------------------------------------

loadModule :: String -> Hugs -> IO ()
loadModule fnms hugs = done

setSearchPath :: String -> Hugs -> IO ()
setSearchPath fnms hugs = done


-- --------------------------------------------------------------------------
--  Hugs Window
-- --------------------------------------------------------------------------

newHugsWindow :: Expect -> IO (Window,Editor String,Box)
newHugsWindow exp = do {
        tp <- newEditor [bg "white", height 32, flexible, wrap WordWrap];
        b <- newVBox[flexible];
        bm <- newHBox ((parent b): opts);
        newScrollBox tp [flexible,parent b];
        newFrame ((parent b): opts);
        win <- window b [text "Hugs WorkBench Interpreter", minSize (cm 11,cm 11)];
        return (win,tp,bm)
} where opts :: (Widget w,HasSize w,HasColour w,HasBorder w) => [Config w]
        opts = [fill Horizontal,height (cm 0.7),relief Groove, borderwidth (cm 0.05),bg "grey"]


                
-- --------------------------------------------------------------------------
--  Hugs Menus
-- --------------------------------------------------------------------------

newHugsMenu :: Hugs -> Editor String -> Box -> IO ()
newHugsMenu hugs @ (Hugs _ win _ el _) tp b = do {
        
        mbl <- newMenuButton [text "File",bg "grey", parent b];
        mnl <- newPulldownMenu mbl [tearOff On];
        newMenuItem mnl [text "Load Files ...", 
                setICmd hugs loadDefinitions "Load Files" "L"];
        newMenuItem mnl [text "Also Files ...",
                setICmd hugs loadAdditionalFiles "Load additional files" ""];
        newMenuItem mnl [text "Project File ...",
                setICmd hugs (loadProject) "Project file" "R"];
        newSeparator [parent mnl];
        newMenuItem mnl [text "Search Path ...",
                setICmd hugs setSearchPath "Search Path" "P"];
        newMenuItem mnl [text "Load Module ...",
                setICmd hugs loadModule "Module Name" ""];
        newSeparator [parent mnl];
        newMenuItem mnl [text "Reload Files",setCmd hugs repeatLastLoad "L"];
        newMenuItem mnl [text "Unload Files",setCmd hugs (loadDefinitions []) ""];
        newSeparator [parent mnl];
        newMenuItem mnl [text "Quit Hugs",setCmd hugs exitHugs "Q"];


        newSpace sp [bg "grey", parent b];
        mbe <- newMenuButton [text "Edit",bg "grey", parent b];
        mne <- newPulldownMenu mbe [tearOff On];
        newMenuItem mne [text "Edit",setCmd hugs editLastFile ""];
        newMenuItem mne [text "Edit File ...",
                setICmd hugs editLoadedFile "Module file" "E"];
        newMenuItem mne [text "Find & Edit Name ...",
                setICmd hugs findDefinition "Name" "" ]; -- TBD

        
        newSpace sp [bg "grey", parent b];
        mbf <- newMenuButton [text "Names",bg "grey", parent b];
        mnf <- newPulldownMenu mbf [tearOff On];
        newMenuItem mnf [text "List All Names", setCmd hugs (listNames []) ""];
        newMenuItem mnf [text "List Names ...",
                setICmd hugs listNames "Names" "N"];
        newMenuItem mnf [text "Info Name ...",
                setICmd hugs displayInfoAboutNames "Names" "M"];


        newSpace sp [bg "grey", parent b];
        mbc <- newMenuButton [text "Commands",bg "grey", parent b];
        mnc <- newPulldownMenu mbc [tearOff On];
        newMenuItem mnc [text "Interrupt", 
                        underline 1,
                        setCmd hugs interruptHugs "C"
                        ];
        newMenuItem  mnc [text "Change Directory ...",
                setICmd hugs changeDirectory "Change Directory" "D"];
        newMenuItem mnc [text "Garbage Collect",setCmd hugs forceGC "G"];
        newMenuItem mnc [text "Shell Escape",
                setICmd hugs shellEscape "Shell command" "S"];
        mn <- newMenuItem mnc [text "Empty Transcript"];

{-
        mne <- getTrigger mn;
        el # registerEH mn (
                mne >>> do { 
                        deleteTextRange tp ((1,0) :: Position) EndOfText;
                        pr <- getPrompt hugs;
                        insertText tp ((1,0) :: Position) pr 
                        }
                );

-}
        newSpace sp [bg "grey", parent b];
        mbo <- newMenuButton [text "Options",bg "grey", parent b];
        mno <- newPulldownMenu mbo [tearOff On];
        newCheckButton [text "Print Statistics",
                        setOption hugs printStatistics, parent mno];
        newCheckButton [text "Print Type after Evaluation",
                        setOption hugs printTypeAfterEval, parent mno];
        newCheckButton [text "Terminate on Error",
                        setOption hugs terminateOnError, parent mno];
        newCheckButton [text "Garbage Collector Notification",
                        setOption hugs gcNotification, parent mno];
        newCheckButton [text "Literate Modules",
                        setOption hugs literateModules, parent mno];
        newCheckButton [text "List Files Loaded",
                        setOption hugs listFilesLoaded, parent mno];
        newCheckButton [text "Display Dots While Loading",
                        setOption hugs displayDotsWhileLoading, parent mno];
        newCheckButton [text "Use Show to Display Results",
                        setOption hugs useShowToDisplayResults, parent mno];
        newCheckButton [text "Show Detailed Kind Errors",
                        setOption hugs detailedKindErrors, parent mno];
        newCheckButton [text "Import Chasing",
                        setOption hugs importChasing, parent mno];
        newSeparator [parent mno];
        newMenuItem mno [text "Set Prompt ...",
                        setICmd hugs setPrompt "Set Prompt" ""
                        ];
        newMenuItem mno [text "Set Repeat String ...",
                        setICmd hugs setRepeatString "Set Repeat String" ""
                        ];              
        newMenuItem mno [text "Set Font ...",
                        setICmd hugs setRepeatString "Set Font" ""
                        ];



        sph <- newSpace sp [bg "grey", fill Horizontal, expand On, parent b];
        mbh <- newMenuButton [text "Help",bg "grey", 
                        side AtRight, anchor East, parent b];
        mnh <- newPulldownMenu mbh [tearOff On];
        newMenuItem mnh [text "List Commands",
                        setCmd hugs (listCommands) ""
                        ];
        newMenuItem mnh [text "List Options",
                        setCmd hugs (listOptions) ""
                        ];
        newMenuItem mnh [text "Tool Info"];


        done

} where sp = cm 0.5


                
-- --------------------------------------------------------------------------
--  Events
-- --------------------------------------------------------------------------

setOption :: Hugs -> (Toggle -> Config Hugs) -> Config (CheckButton ())
setOption hugs @ (Hugs _ win _ el _) option butt = do {
        butt # command (\toggle -> do {
                        configure hugs [option toggle];
                        done
                        });
        bind el (triggered butt >>> done);
        return butt
}


setCmd :: Hugs -> (Hugs -> IO ()) -> String -> Config (Button ())
setCmd hugs  @ (Hugs _ win _ el _) cmd "" butt = do 
        butt # command (\() -> cmd hugs);
        bind el (triggered butt >>> done)
        return butt
setCmd hugs @ (Hugs _ win _ el _) cmd key butt = do 
        butt # command (\() -> cmd hugs);
        bind el (
                        triggered butt >>> done
                  +>    keystroke butt (Control,key) >>> cmd hugs
                )
        configure butt [accelerator ("Ctrl-" ++ key)]           


setICmd :: GUIValue a 
        => Hugs -> (a -> Hugs -> IO ()) -> String -> String -> Config (Button ())
setICmd hugs @ (Hugs _ win _ el _) cmd title _ butt = do
        butt # command return;
        bind el (triggered butt >>> 
           (forkIO (doRequest) >>= \id -> return () ))
        return butt
 where doRequest = do {
        (x,y) <- getPosition win;
        pwin <- newPromptWin title cdefault [
                        modal True, 
                        transient win,
                        size (cm 10,cm 5),
                        position (x + (cm 4),y + (cm 4))
                        ];
        forkDialog pwin (\ans -> 
           case ans of
                Nothing -> done
                (Just val) -> cmd val hugs
          )
}
                
