{- #########################################################################

MODULE        : GUIRealise
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Commands for realising widgets and windows, i.e. for
                packing them onto the screen. The commands are generated
                from the basis of the local cache and forwarded to wish
                in the form of Tcl scripts.

NOTE            packXXX         - packs the item XXX
                realiseXXX      - constructs the packing commands for XXX
                displayXXX      - displays XXX by forwarding the packing
                                  commands to Tk.
                

   ######################################################################### -}


module GUIRealise (
        displayWindow,
        packWidget,
        packMenu,
        packMenuItem,
        packCanvasItem,
        packTextWindowItem,
        packTextTagItem,

        objectNotPacked,
        parentNotPacked,

        objectNotPacked,
        parentNotPacked
        
        ) where

import Concurrency
import FiniteMap

import Resources
import GUIObject
import GUIState
import GUIWish
import Debug(debug)


-- --------------------------------------------------------------------------
--  Packing/Display of Windows
-- --------------------------------------------------------------------------

displayWindow :: Maybe GUIOBJECT -> GUIOBJECT -> IO ()
displayWindow Nothing win = do {
        (cmds,release) <- realiseWidget win (ObjectName  "");
        ans <- try (execWish (execScript cmds));
        release;
        propagate ans
        }
displayWindow (Just pid) wid = 
        displayWidget pid wid 


-- --------------------------------------------------------------------------
--  Packing of Widgets
-- --------------------------------------------------------------------------

packWidget :: GUIOBJECT -> GUIOBJECT -> Maybe Methods -> IO ()
packWidget pid cid m = packGUIObject pid cid m (displayWidget pid cid)                  

displayWidget :: GUIOBJECT -> GUIOBJECT -> IO ()
displayWidget pid wid = displayGUIObject pid wid realiseWidget


realiseWidget :: GUIOBJECT -> ObjectName -> IO Realise
realiseWidget wid @ (GUIOBJECT key mon) op = do {
        (np,t,co,cp,chs,m,b) <- updObjectName wid (mkWidgetName op key);
        (cmds',release') <- realiseChildren wid chs np t;
        create <- return ((createCmd m) t np key co);
        pack <- return ((packCmd m) t op np cp key b);
        return (create ++ pack ++ cmds', release mon >> release')
}       

realiseChildren :: GUIOBJECT -> [GUIOBJECT] -> ObjectName -> ObjectKind -> IO Realise
realiseChildren wid chs np MENU = realiseMenuItems wid chs np
realiseChildren wid chs np CANVAS = realiseCanvasItems chs np 
realiseChildren wid chs np (TEXT _) = realiseTextItems chs np 
realiseChildren wid chs np _ = realiseWidgets chs np 
     

realiseWidgets :: [GUIOBJECT] -> ObjectName -> IO Realise
realiseWidgets [] _ = return ([],done)
realiseWidgets (ch : chs) path = do {
        (cmds1,release1) <- realiseWidget ch path;
        (cmds2,release2) <- realiseWidgets chs path;
        return (cmds1 ++ cmds2,release1 >> release2)
        }


mkWidgetName :: ObjectName -> ObjectID -> OST -> (OST,(ObjectName,ObjectKind,[ConfigOption],[ConfigOption],[GUIOBJECT],Methods,[Binding]))
mkWidgetName onm key (OST t _ o p c b f m) = 
        (OST t (Just nm) o p c b f m,(nm,t,fmToList o,fmToList p,c,m,b))
 where  nm = append onm key
        append :: ObjectName -> ObjectID -> ObjectName
        append (ObjectName path) (ObjectID no)  = 
                ObjectName (path ++ ".w" ++ (show no))
        append (MenuItemName oname _) oid  = append oname oid


                      
-- --------------------------------------------------------------------------
--  Packing of Menu Items
-- --------------------------------------------------------------------------

packMenuItem :: GUIOBJECT -> GUIOBJECT -> Maybe Methods -> IO ()
packMenuItem pid cid m = packGUIObject pid cid m (displayMenuItem pid cid)                      

displayMenuItem :: GUIOBJECT -> GUIOBJECT -> IO ()
displayMenuItem menu item = displayGUIObject menu item displayItem
        where displayItem item pnm = do {
                        childs <- getChildObjects menu;
                        toff <- cget menu "tearoff" ;
                        realiseMenuItem item pnm (length childs + toff - 1);    
                        }
        {- the - 1 is needed because the current item has already been
                registered as a child -}
        

realiseMenuItems :: GUIOBJECT -> [GUIOBJECT] -> ObjectName -> IO Realise
realiseMenuItems menu chs np = do {
        toff <- cget menu "tearoff" ;
        realiseMenuItemList chs np toff
}


realiseMenuItemList :: [GUIOBJECT] -> ObjectName -> Int -> IO Realise
realiseMenuItemList [] _ _ = return ([],done)
realiseMenuItemList (ch : chs) path eno = do {
        (cmds1,release1) <- realiseMenuItem ch path eno;
        (cmds2,release2) <- realiseMenuItemList chs path (eno + 1);
        return (cmds1 ++ cmds2,release1 >> release2)
}


realiseMenuItem :: GUIOBJECT -> ObjectName -> Int -> IO Realise
realiseMenuItem wid @ (GUIOBJECT key mon) op eno = do { 
        (enm,t,co,chs,m,b) <- updObjectName wid (mkMenuItemName op eno);
        (cmds',release') <- realiseItems t chs enm eno;
        create <- return ((createCmd m) t op key co);
        pack <- return ((packCmd m) t op enm [] key b);
        return (create  ++ pack ++ cmds', release mon >> release')
}

realiseItems :: ObjectKind -> [GUIOBJECT] -> ObjectName -> Int -> IO Realise
realiseItems MENUBUTTON [cid] enm eno = realiseCascadeMenu cid enm eno
realiseItems _ _ _ _ = return ([],done)


realiseCascadeMenu :: GUIOBJECT -> ObjectName -> Int -> IO Realise
realiseCascadeMenu wid @ (GUIOBJECT key mon) pname eno = do {
        (np,t,co,cp,chs,m,b) <- updObjectName wid (mkWidgetName pname key);
        (cmds',release') <- realiseMenuItems wid chs np;
        create <- return ((createCmd m) t np key co);
        pack <- return ((packCmd m) MENU pname np [] (ObjectID eno) []);
        return (create ++ pack ++ cmds', release' >> release mon)
}


mkMenuItemName :: ObjectName -> Int -> OST ->
                 (OST,(ObjectName,ObjectKind,[ConfigOption],[GUIOBJECT],Methods,[Binding]))
mkMenuItemName nm eno (OST t _ o p c b f m) = 
        (OST t (Just enm) o p c b f m,(enm,t,fmToList o,c,m,b))
        where enm = MenuItemName nm eno



                
-- --------------------------------------------------------------------------
-- Pack Menu
-- --------------------------------------------------------------------------   

packMenu :: GUIOBJECT -> GUIOBJECT -> IO ()
packMenu mb mn = do {
        name <- getObjectName mb;
        case name of
                (Just (MenuItemName _ eno)) -> 
                        displayGUIObject mb mn (\menu name -> realiseCascadeMenu menu name eno)
                _ -> packGUIObject mb mn Nothing (displayWidget mb mn)
        }

                      
-- --------------------------------------------------------------------------
--  Packing of Canvas Items
-- --------------------------------------------------------------------------

packCanvasItem :: GUIOBJECT -> GUIOBJECT -> Maybe Methods -> IO ()
packCanvasItem pid cid m = packGUIObject pid cid m (displayCanvasItem pid cid)


displayCanvasItem :: GUIOBJECT -> GUIOBJECT -> IO ()
displayCanvasItem canvas item = displayGUIObject canvas item realiseCanvasItem


realiseCanvasItems :: [GUIOBJECT] -> ObjectName -> IO Realise
realiseCanvasItems [] cname = 
        return ([],done)
realiseCanvasItems (item:chs) cname = do {
        (cmds1,release1) <- realiseCanvasItem item cname;
        (cmds2,release2) <- realiseCanvasItems chs cname;
        return (cmds1 ++ cmds2,release1 >> release2)
}


realiseCanvasItem :: GUIOBJECT -> ObjectName -> IO Realise
realiseCanvasItem item@(GUIOBJECT oid mon) cnm = do {
        (cinm,t,args,cp,chs,m,b) <- updObjectName item (mkCanvasItemName cnm oid);
        (cmd',release') <- realiseEmbeddedItems item t cnm cinm chs m;
        create <- return ((createCmd m) t cinm oid args);
        pack <- return ((packCmd m) t cnm cinm args oid b);
        return ( create  ++ cmd' ++ pack,release mon >> release')
}


realiseEmbeddedItems :: GUIOBJECT -> ObjectKind -> ObjectName -> ObjectName -> 
                        [GUIOBJECT] -> Methods -> IO Realise
realiseEmbeddedItems item (CANVASITEM EMBEDDEDCANVASWIN _) cnm cinm [win] m = do {
        (cmd',release') <- realiseWidget win cnm;
        mwnm <- getObjectName win;
        case mwnm of 
                Nothing -> return ([], done)
                (Just wnm) -> 
                        return (cmd' ++ ((csetCmd m) cinm (winname wnm)),release')  
        } where winname wnm = [("window",toGUIValue(toWidgetName wnm))]
realiseEmbeddedItems _ _ _ _ _ _ = return ([],done)


mkCanvasItemName :: ObjectName -> ObjectID -> OST -> 
  (OST,(ObjectName,ObjectKind,[ConfigOption],[ConfigOption],[GUIOBJECT],Methods,[Binding]))
mkCanvasItemName cnm oid (OST t _ o p c b f m) = 
        (OST t (Just nm) o p c b f m,(nm,t,fmToList o,fmToList p,c,m,b))
        where nm = CanvasItemName cnm (CanvasTagOrID oid)


                                              
-- --------------------------------------------------------------------------
--  Display of Text Items
-- --------------------------------------------------------------------------

realiseTextItems :: [GUIOBJECT] -> ObjectName -> IO Realise
realiseTextItems [] cname = 
        return ([],done)
realiseTextItems (item:chs) tpname = do {
        k <- getObjectKind item;
        (cmds1,release1) <- realiseTextItem k item tpname;
        (cmds2,release2) <- realiseTextItems chs tpname;
        return (cmds1 ++ cmds2, release1 >> release2)
}


realiseTextItem :: ObjectKind -> GUIOBJECT -> ObjectName -> IO Realise
realiseTextItem (TEXTTAG _) item tpname = realiseTextTagItem item tpname
realiseTextItem _ item tpname = realiseTextWindowItem item tpname

                                              
-- --------------------------------------------------------------------------
--  Display of Text Tags
-- --------------------------------------------------------------------------

packTextTagItem :: GUIOBJECT -> GUIOBJECT -> Maybe Methods -> IO ()
packTextTagItem pid cid m = packGUIObject pid cid m (displayTextTagItem pid cid)


displayTextTagItem :: GUIOBJECT -> GUIOBJECT -> IO ()
displayTextTagItem tp twin = displayGUIObject tp twin realiseTextTagItem


realiseTextTagItem :: GUIOBJECT -> ObjectName -> IO Realise
realiseTextTagItem twin@(GUIOBJECT oid mon) tnm = do {
        (wnm,k,args,m) <- updObjectName twin (mkTextTagName tnm oid);
        create <- return ((createCmd m) k wnm oid []);
        set <- return ((csetCmd m) wnm args);
        return (create ++ set, release mon)
        }


mkTextTagName :: ObjectName -> ObjectID -> OST ->
                         (OST,(ObjectName,ObjectKind,[ConfigOption],Methods))
mkTextTagName tnm oid (OST k _ o p c b f m) = 
        (OST k (Just nm) o p c b f m,(nm,k,fmToList o,m))
        where nm = TextPaneItemName tnm (TextTagID oid)
              inx (TEXTTAG i) = i
              

-- --------------------------------------------------------------------------
--  Packing of Embedded Text Windows
-- --------------------------------------------------------------------------

packTextWindowItem :: GUIOBJECT -> GUIOBJECT -> Maybe Methods -> IO ()
packTextWindowItem pid cid m = 
        packGUIObject pid cid m (displayTextWindowItem pid cid)



displayTextWindowItem :: GUIOBJECT -> GUIOBJECT -> IO ()
displayTextWindowItem tp twin = displayGUIObject tp twin realiseTextWindowItem



realiseTextWindowItem :: GUIOBJECT -> ObjectName -> IO Realise
realiseTextWindowItem twin @ (GUIOBJECT oid mon) tnm = do {
        (wnm,k,args,chs,m) <- updObjectName twin (mkTextWinName tnm oid);
        (cnm,cmd',release') <- realiseEmbeddedTextWidget (head chs) tnm;
        setObjectName twin (TextPaneItemName tnm (EmbeddedWindowName cnm));
        create <- return ((createCmd m) k wnm oid []);
        set <- return ((csetCmd m) wnm (("window",winname cnm):args));  
        return ( create ++ cmd' ++ set, release mon >> release')
} where winname = toGUIValue . toWidgetName


realiseEmbeddedTextWidget :: GUIOBJECT -> ObjectName -> IO (ObjectName,[TclCmd],IO ())
realiseEmbeddedTextWidget cid tnm = do {
        (wscr,release) <- realiseWidget cid tnm;
        mcnm <- getObjectName cid;
        case mcnm of 
                (Just cnm) -> return (cnm,wscr,release)
}


mkTextWinName :: ObjectName -> ObjectID -> OST -> 
  (OST,(ObjectName,ObjectKind,[ConfigOption],[GUIOBJECT],Methods))
mkTextWinName tnm oid (OST k _ o p c b f m) = 
        (OST k (Just nm) o p c b f m,(nm,k,fmToList o,c,m))
        where nm = TextPaneItemName tnm (TextItemPosition (inx k))
              inx (EMBEDDEDTEXTWIN i) = i
              

        
-- --------------------------------------------------------------------------
-- Utility Functions
-- --------------------------------------------------------------------------

packGUIObject :: GUIOBJECT -> GUIOBJECT -> Maybe Methods -> IO () -> IO ()
packGUIObject pid cid meth cmd = do{
        mpid <- getParentObjectID cid;
        case mpid of 
                (Just pid) -> done      {- can only be packed once -} 
                _ ->  do {
                        incase meth (setMethods cid);
                        makeChildObject pid cid;
                        cmd                     
                        }
}

type RealiseCmd = GUIOBJECT ->  ObjectName -> IO Realise

type Realise = (TclScript, IO ())

displayGUIObject :: GUIOBJECT -> GUIOBJECT -> RealiseCmd -> IO ()
displayGUIObject parent child cmd = do {
        mpnm <- getObjectName parent;
        case mpnm of
                Nothing -> done
                (Just pnm) -> do {
                        (script,release) <- cmd child pnm;
                        ans <- try (execWish (execScript script));
                        release;
                        propagate ans
                        }
}

updObjectName :: GUIOBJECT -> (OST -> (OST,a)) -> IO a
updObjectName (GUIOBJECT _ rv) f = do {
        acquire rv;
        updVar' rv f
        }

        
-- --------------------------------------------------------------------------
-- IOErrors
-- --------------------------------------------------------------------------

objectNotPacked :: IOError
objectNotPacked = userError "gui object is not packed"

parentNotPacked :: IOError
parentNotPacked = userError "parent gui object is not packed"

