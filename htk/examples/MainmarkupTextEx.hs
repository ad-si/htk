-- -----------------------------------------------------------------------
--
-- $Source$
--
-- HTk - a GUI toolkit for Haskell  -  (c) Universitaet Bremen
--
-- $Revision$ from $Date$  
-- Last modification by $Author$
--
-- -----------------------------------------------------------------------

module Main(main) where

import HTk
import MarkupText

main :: IO ()
main =
  do
    htk <- initHTk [text "markup text example"]

    ed <- newEditor htk [size (80,25), state Disabled] ::
            IO (Editor String)
    pack ed [Fill Both, Expand On]

    but <- newButton ed [text "This is an embedded button widget"]
             :: IO (Button String)

    let
      link :: String -> MarkupText
      link str = flipcolour "#4756ff" "#4c90ff"
                   [rangeaction (Just (ed # cursor arrow >> done))
                                (Just (ed # cursor xterm >> done))
                      [flipunderline [prose str]]]

      txt :: [MarkupText]
      txt =
        [newline,
         prose "This is prose.",
         newline,
         newline,
         newline,
         newline,
         bgcolour "yellow" [prose "Fonts: "],
         spaces 1,
         MarkupText.font (Courier, 16::Int) [prose "Courier,"],
         spaces 1,
         MarkupText.font (Times, 16::Int) [prose "Times,"],
         spaces 1,
         MarkupText.font (Lucida, 14::Int) [prose "Lucida,"],
         prose " ...",
         newline,
         newline,
         MarkupText.font (Courier, 16::Int)
           [bgcolour "green" [prose "Font attributes:"],
            spaces 1,
            bold [prose "BOLD"],
            prose ", ",
            italics [prose "italics"],
            prose ", ",
            MarkupText.underline [prose "underline"],
            prose " ..."],

{- bug in font module (problem: parse of default font fails in getFont):
         newline,
         prose "Font attributes: ",
         bold [prose "BOLD"],
-}

         newline,
         newline,
         action (putStrLn "action1")
           [link "This is an action, click here!"],
         newline,
         newline,
         clipup
           [link "This is a clipup"]
           [leftmargin 10
              [prose "This is the text that ",
               bgcolour "blue" [prose "clips"],
               prose " up.",
               newline,
               action (putStrLn "action2")
                 [link "This is another action."],
               newline,
               clipup [link "This is a cascaded clipup."]
                 [leftmargin 20
                    [prose "This is the text of the ",
                     colour "green" [prose "cascaded"],
                     prose " clipup."]],
               prose "Another line of prose"]],
         newline,
         bgcolour "yellow" [prose "Special characters:"],
         newline,
         forall, prose ", ",
         _Forall, prose ", ",
         exists, prose ", ",
         _Exists, prose ", ",
         alpha, prose ", ",
         beta, prose ", ",
         chi, prose ", ",
         _Alpha, prose ", ",
         _Beta, prose ", ",
         _Chi, prose ", ",
         newline,
         equiv, prose ", ",
         neq, prose ", ",
         leq, prose ", ",
         MarkupText.not, prose ", ",
         MarkupText.and, prose ", ",
         MarkupText.or, prose ", ",
         MarkupText.sum,
         newline, newline,
         href [link "A link to another MarkupText"] txt2,
         newline, newline{-,
         window but-}]

      add_txt :: [MarkupText]
      add_txt =
         [MarkupText.font (Times, 16::Int)
            [prose "This ",
             colour "red" [prose "text"],
             prose " has been ",
             bold [prose "inserted later"],
             prose "."],
          newline,
          prose "This is another line of inserted text."]

      txt2 :: [MarkupText]
      txt2 =
        [newline,
         prose "This is another markup Text!",
         newline,
         prose "When you go back the inserted text will not be there anymore!",
         newline,
         newline,
         href [link "Back"] txt]

    ed # new txt
    ed # insertAt add_txt (4,5)

    (htk_destr, _) <- bindSimple htk Destroy
    sync (htk_destr)
