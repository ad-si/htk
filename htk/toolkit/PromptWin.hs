{- #########################################################################

MODULE        : PromptWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Basic input window


   ######################################################################### -}


module PromptWin (
        module Prompt,

        PromptWin,

        newPromptDialog,
        newPromptWin

        ) where

import Concurrency
import HTk
import Message
import Separator
import Focus
import Button
import Font
import Space
import SelectBox
import ModalDialog
import Prompt
import DialogWin
import Entry
import Future
import Debug(debug)



-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

data PromptWin a = PromptWin (Future a)


-- --------------------------------------------------------------------------
-- Data Type
-- --------------------------------------------------------------------------

newPromptWin :: GUIValue a => String -> a -> [Config Window] -> IO (PromptWin (Maybe a))
newPromptWin str val wol = do {
        b <- newVBox [
                fill Horizontal, 
                relief Raised, 
                borderwidth (cm 0.05) 
                ];
        newVBox [flexible,parent b];    -- provide fill area
        newMessage [pad Horizontal (cm 0.5), 
                value str::Config(Message String), 
                flexible, 
                width (cm 10), 
                justify JustCenter,
                font fmsg,
                parent b
                ];
        newSpace (cm 0.3) [parent b];
        newSeparator [fill Horizontal,parent b];
        newSpace (cm 0.3) [parent b];
        ent <- newEntry [
                pad Horizontal (cm 0.5), 
                fill Horizontal, 
                value val,
                parent b
                ];
        newSpace (cm 0.3) [parent b];
        newSeparator [fill Horizontal,parent b];
        newSpace (cm 0.3) [parent b];           
        sb <- newSelectBox (Just 0) [fill Horizontal,anchor Center,parent b];
                newButton [
                        (text "Ok"::Config(Button Bool)),
                        pad Horizontal (cm 0.5),
                        command (\() -> return True), 
                        side AtLeft,
                        expand On,
                        parent sb
                        ];
                newButton [
                        (text "Cancel"::Config(Button Bool)),
                        command (\() -> return False),
                        pad Horizontal (cm 0.5),
                        side AtLeft,
                        expand On,
                        parent sb
                        ];
        newSpace (cm 0.3) [parent b];
        win <- window b ([text "Prompt Window", sizeFrom Program] ++ wol);
        fut <- newFuture ( do {
                ans <- tryUntilOK (fetchValue win sb ent);
                try(destroy win);
                return ans
                });
        return (PromptWin fut)
} where fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}


-- --------------------------------------------------------------------------
-- Window Constructor
-- --------------------------------------------------------------------------

newPromptDialog :: GUIValue a => String -> a -> [Config Window] -> IO (Maybe a)
newPromptDialog str val wol = do
        dlg <- newPromptWin str val wol
        sync (triggered dlg)



-- --------------------------------------------------------------------------
-- Instantiations
-- --------------------------------------------------------------------------

instance Reactive PromptWin a where
        triggered (PromptWin fut) = lift (receive fut)


-- --------------------------------------------------------------------------
-- Utility Function: Reading reply
-- --------------------------------------------------------------------------


fetchValue :: GUIValue a => Window -> SelectBox Bool -> Entry a -> IO (Maybe a)
fetchValue win sb ent = tryUntilOK ( do {
        sbe <- getTrigger sb;
        ans <- modalInteraction win False (events win sbe);
        if ans then do {
                ans <- try(getValue ent);
                case ans of
                        (Left e) -> do {
                                newErrorWin ("illegal entry value") [
                                        transient win,modal True];
                                fetchValue win sb ent
                                }
                        (Right jv) -> return (Just jv)
                }

        else
                return Nothing
        }) where events win sbe =
                        sbe
                   +>   destroyed win >>> return False  
                   +>   keyPressed win "Return" >>> return True
