{- #########################################################################

MODULE        : HtmlKernel
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : The Html Kernel.

   ######################################################################### -}


module HtmlKernel (
        WWW(..),
        Monad(..),
        runWWW,

        module HtmlTypes,
        module HtmlClasses,

        setHtmlAttr,
        getHtmlAttr,

        getHtmlGUIObject,

        createHtmlElem,
        createHtmlComp,
        addHtmlElem 
        )

where
import Computation
import HTk
import HtmlTypes
import HtmlClasses
import Dynamics
import Thread
import Debug(debug)



-- --------------------------------------------------------------------------
-- WWW Monad
-- --------------------------------------------------------------------------

type WWW a = IOS HtmlDoc a

runWWW :: WWW a -> IO (HtmlDoc, a)
runWWW c = execIOS c (HtmlDoc [])


-- --------------------------------------------------------------------------
-- Elements and their Attributes
-- --------------------------------------------------------------------------

setHtmlAttr :: (HtmlElem w,GUIValue a) => w -> String -> a -> IO w
setHtmlAttr w cid v = return (fromHtmlElem (HtmlElement k (a:al) el))
        where   (HtmlElement k al el) = (toHtmlElem w)
                a = HtmlAttribute cid (show v)


getHtmlAttr :: (HtmlElem w,GUIValue a) => w -> String -> IO a
getHtmlAttr w cid = 
        case filter (\(HtmlAttribute cid' _) -> cid' == cid) al of 
                [] -> return  cdefault
                ((HtmlAttribute _ v) : _) -> return (read v)
        where (HtmlElement k al el) = (toHtmlElem w)

getHtmlGUIObject :: GUIOBJECT
getHtmlGUIObject = error "toGUIObject not defined for Html Elements"


configureHtmlElem :: HtmlElem w => HtmlKind -> [Config w] -> IO w 
configureHtmlElem k cnfs = configureHtml (fromHtmlElem elem) cnfs
        where elem = HtmlElement k [] []
              configureHtml :: HtmlElem w => w -> [Config w] -> IO w
              configureHtml elem [] = return elem       
              configureHtml elem (c:cl) = do {
                elem' <- c elem;
                configureHtml elem' cl
                }


createHtmlElem ::  HtmlElem w => HtmlKind -> [Config w] -> WWW ()
createHtmlElem k cnfs = do {
        (HtmlDoc el) <- getStateM;
        e <- embedIOS (configureHtmlElem k cnfs);
        setStateM (HtmlDoc ((toHtmlElem e) : el))
        }


createHtmlComp ::  HtmlElem w => HtmlKind -> [Config w] -> WWW a -> WWW a
createHtmlComp k cnfs www = do {
        (HtmlDoc el) <- getStateM;
        (HtmlDoc comp,val) <- embedIOS (runWWW www);
        e <- embedIOS (configureHtmlElem k cnfs);
        (let (HtmlElement k al _) = (toHtmlElem e) in   
                setStateM (HtmlDoc ((HtmlElement k al (reverse comp)) : el)));
        return val
        }


addHtmlElem :: HtmlKind -> WWW ()
addHtmlElem k = do {
        (HtmlDoc el) <- getStateM;
        setStateM (HtmlDoc (e : el))
} where e = HtmlElement k [] []


        
