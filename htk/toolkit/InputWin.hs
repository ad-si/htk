{- #########################################################################

MODULE        : InputWin
AUTHOR        : Einar Karlsen,  
                University of Bremen
                email:  ewk@informatik.uni-bremen.de
DATE          : 1996
VERSION       : alpha
DESCRIPTION   : Basic input window for record values and their fields.
                The third argument to inputWin is a is-well-formedness
                function that is finally called to ensure that the entered
                record value is legal. The legality of the individual fields
                is checked by getValue!

TO BE DONE    : The handling of Returns within a form must be considered.

                In presense of text fields, these should normally be
                flexible rather than the header message!!


   ######################################################################### -}


module InputWin (
        module InputForm,
        forkDialog,

        InputWin,
        newInputWin,
        newInputDialog

        ) where

import Concurrency
import HTk
import Message
import Separator
import Keyboard
import Button
import Font
import Space
import SelectBox
import ModalDialog
import InputForm
import DialogWin
import Debug(debug)


-- ---------------------------------------------------------------------------
-- Data Type
-- ---------------------------------------------------------------------------

data InputWin a = InputWin (Future a)

-- ---------------------------------------------------------------------------
-- Instantiations
-- ---------------------------------------------------------------------------

instance Reactive InputWin a where
        triggered (InputWin fut) = lift(receive fut)

-- ---------------------------------------------------------------------------
-- Constructor
-- ---------------------------------------------------------------------------

newInputWin :: String -> InputForm a -> Maybe (a -> IO (Maybe String)) -> 
                [Config Window] -> IO (InputWin (Maybe a))
newInputWin str form iswf wol = do {
        b <- newVBox [fill Horizontal, relief Raised, borderwidth (cm 0.05) ];
        newVBox [flexible,parent b];    -- provide fill area
        newMessage [pad Horizontal (cm 0.5), 
                (value str)::Config(Message String), 
                flexible, 
                width (cm 10), 
                justify JustCenter,
                font fmsg,
                parent b
                ];
        newSpace (cm 0.3) [parent b];
        newSeparator [fill Horizontal,parent b];
        newSpace (cm 0.3) [parent b];
        configure form [parent b, pad Horizontal (cm 0.5), fill Horizontal];
        newSpace (cm 0.3) [parent b];
        newSeparator [fill Horizontal,parent b];
        newSpace (cm 0.3) [parent b];           
        sb <- newSelectBox Nothing [fill Horizontal, anchor Center,parent b];
                newButton [
                        (text "Ok" :: Config (Button Bool)),
                        pad Horizontal (cm 0.5),
                        command (\() -> return True), 
                        parent sb,
                        side AtLeft
                        ];
                newButton [
                        (text "Cancel" :: Config (Button Bool)),
                        command (\() -> return False),
                        pad Horizontal (cm 0.5),
                        parent sb,
                        side AtRight
                        ];
        newSpace (cm 0.3) [parent b];
        win <- window b ([text "Input Form Window", sizeFrom Program] ++ wol);
        fut <- newFuture ( do {
                ans <- fetchValue win sb form iswf;
                destroy win;
                return ans
                });
        return (InputWin fut)
} where fmsg = xfont {family = Just Times, weight = Just Bold, points = (Just 180)}


-- ---------------------------------------------------------------------------
-- Dialog
-- ---------------------------------------------------------------------------

newInputDialog :: String -> InputForm a -> Maybe (a -> IO (Maybe String)) -> 
                [Config Window] -> IO (Maybe a)
newInputDialog  str form iswf wol = do
        iwin <- newInputWin str form iswf wol
        sync(triggered iwin)

-- ---------------------------------------------------------------------------
-- Utility Function
-- ---------------------------------------------------------------------------

fetchValue :: Window -> SelectBox Bool -> InputForm a -> 
                Maybe (a -> IO (Maybe String)) -> IO (Maybe a)
fetchValue win sb form iswf = do {
        sbe <- getTrigger sb;
        ans <- modalInteraction win False (
                        sbe
                   +>   destroyed win >>> return False
--                 +>   keyPressed win "Return" >>> return True 
                );
        if ans then do {
                val <- try (getValue form);
                checkInput iswf val;
                }
        else
                return Nothing
} where checkInput _ (Left e) = fetchValue win sb form iswf  -- illegal field
        checkInput Nothing (Right val) = return (Just val)   -- no check needed
        checkInput (Just iswf) (Right val) = do {            -- check value
                msg <- iswf val;
                case msg of
                        Nothing -> return (Just val)         -- value ok
                        (Just msg) -> do {
                                newErrorWin msg [];
                                fetchValue win sb form (Just iswf)  -- try again
                                }
                }
