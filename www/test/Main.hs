module Main (
        main 
        ) 
where

import Concurrency
import HTk
import Html
import Time(toCalendarTime, getClockTime, ClockTime, CalendarTime(..))
import TableGen
import Date
import Debug(debug)

main :: IO ()
main =  runtest test6 "test.www"                                >>
        done

runtest script fname = do {
        html <- generateHtml script;
        saveHtml html fname;
        str <- unparseHtml html;
        putStr (str ++ "\n");
        done
}       


-- --------------------------------------------------------------------------
-- TEST16
-- --------------------------------------------------------------------------

test6 = generateTable "HTML Modules" elems bconfs tconfs
        where tconfs = [frames FrameBorder, tableborder, cellpadding 10]
              bconfs = [background "white", foreground "blue"]

elems :: [HDescr]
elems = [       
        HDescr "HtmlApplet" (Date 1997 06 17) Literal ["Applet Element: applet, codefile, param"],
        HDescr "HtmlBlock" (Date 1997 06 17) Literal ["Block Elements: body, address, division, heading, br, hr, paragraph"],
        HDescr "HtmlClasses" (Date 1997 06 17) Literal ["Html Specific Classes"],
        HDescr "HtmlForm" (Date 1997 06 17) Literal ["Input Form Elements: form, hselect, option, textarea, input"],
        HDescr "HtmlFrame" (Date 1997 06 17) Literal ["Frame Elements: hframe, frameset, noframes"],
        HDescr "HtmlLink" (Date 1997 06 17) Literal ["Hypertext Link Elements: img, anc, href, imagemap, area"],
        HDescr "HtmlList" (Date 1997 06 17) Literal ["List Elements: li, dt, dd, deflist, dirlist, menulist, ulist, olist"],
        HDescr "HtmlMeta" (Date 1997 06 17) Literal ["Meta Elements: prologue, header, doctitle, base, isindex, link, meta"],
        HDescr "HtmlTable" (Date 1997 06 17) Literal ["Table Elements: table, caption, th, td, tr"],
        HDescr "HtmlText" (Date 1997 06 17) Literal ["Text Elements: comment, plain, sty, format, basefont"]
        ]


data HDescr =  HDescr {fName :: String, fDate :: Date, fKind :: HKind, fText :: [String]}

data HKind = Literal | PlainText deriving (Eq,Ord,Show,Read)

instance GUIValue HKind

instance TableCell HKind
instance TableCell Date

instance TableRow HDescr where
   mkTableHeader _ =  do {tr []; sequence_ (fmap (\s -> do {th []; plain s}) sl)}
        where sl = ["Module Name", "Date", "Kind", "Description"]
   mkTableRow hd = do {
                tr [];
                td []; mkTableCell (fName hd);
                td []; mkTableCell (fDate hd);
                td []; mkTableCell (fKind hd);
                td []; mkTableCell (fText hd)
                }
