{- #########################################################################

MODULE        : Html
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1997
VERSION       : alpha
DESCRIPTION   : Html Documents.

CAVEATS       : 

The configuration options of the GUI and DaVinci have been reused as
far as possible. This means that some of the configure options do not
have the same names and values of those we are used to from Html. In
particular:

1) the align option is represented by the class HasJustity whenever the
values are allowed to be left, center and right.

2) the valing option is then represented by the class HasVerticalAlign.

The class HasAlign is rarely used because the values of tye Alignment
does not fit very well with whats expected by HTML (which is quite messy
in this are).

Confused? 

The pre command is known not to work. The unparsing algorithm must be 
changed to aboid inserting blanks and newlines within a PRE block.


Html defines loads of vaious ways of referencing other urls (href, src, img
etc). The current definition provide such hyperlink throug an instantiation
of class HtmlHyperLink which proivde a single url configuration option
for designating url's.



   ######################################################################### -}



module Html (

        GUIVALUE,
        Config,
        configure,
        GUIValue(..),

        Colour(..), 
        Justify(..),
        Alignment(..),
        Font(..),

        GUIObject(..),
        HasColour(..),
        HasSize(..),
        HasText(..),
        HasName(..),
        HasJustify(..),
        HasBorder(..),

        fg,bg,foreground,background,

        Distance,
        cm, pp, mm, ic, tocm, toinch,

        WWW,
        Monad(..),
        runWWW,
        HtmlDoc,
        generateHtml,
        unparseHtml,
        saveHtml,
        includeHtml,

        module HtmlClasses,
        module HtmlApplet,
        module HtmlBlock,
        module HtmlForm,
        module HtmlLink,
        module HtmlList,
        module HtmlMeta,
        module HtmlText,
        module HtmlTable,
        module HtmlFrame,

        hattr

        )

where

import HTk
import Colour

import HtmlKernel
import HtmlClasses
import HtmlTypes
import HtmlList
import HtmlBlock
import HtmlText
import HtmlLink
import HtmlMeta
import HtmlForm
import HtmlApplet
import HtmlTable
import HtmlFrame
import Debug(debug)


-- --------------------------------------------------------------------------
-- Html Document Commands
-- --------------------------------------------------------------------------

generateHtml :: WWW a -> IO HtmlDoc
generateHtml www = do {(html,_) <- runWWW www; return html}     


unparseHtml :: HtmlDoc -> IO String
unparseHtml html = return (show html)


saveHtml :: HtmlDoc -> String -> IO ()
saveHtml html fname =  do {str <- unparseHtml html; writeFile fname str}


includeHtml :: FilePath -> WWW ()
includeHtml fname = do {cnt <- embedIOS(readFile fname); plain cnt}


-- --------------------------------------------------------------------------
-- Attribute Command
-- --------------------------------------------------------------------------

hattr :: HtmlElem e => String -> String -> Config e
hattr aid val e = setHtmlAttr e aid val
