
module Main(main) where

import HTk.Toplevel.HTk
import HTk.Toolkit.MarkupText as MarkupText

main :: IO ()
main =
  do
    htk <- initHTk [text "markup text example", size (500, 400)]

    ed <- newEditor htk [size (80,5), state Disabled]
    pack ed [Fill Both, Expand On]

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
         bold [prose "BOLD"],
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
         newline,
         newline,
         action (putStrLn "action1")
           [link "This is an action, click here!"],
         newline,
         prose "This text is ",
         MarkupText.offset 10 [prose "raised"],
         prose " and ",
         MarkupText.offset (-10) [prose "lowered."],
         prose ".",
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
         centered [prose "This line centered...\nand this as well?!?"], newline,
         newline,
         forallsmall, prose ", ",
         forallbig, prose ", ",
         exists, prose ", ",
         eexists, prose ", ",
         alpha, prose ", ",
         beta, prose ", ",
         chi, prose ", ",
         aalpha, prose ", ",
         bbeta, prose ", ",
         cchi, prose ", ",
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
         newline, newline,
         window (do
                   but <- newButton ed
                            [text "This is an embedded button widget",
                             cursor arrow]
                   clickedbut <- clicked but
                   death <- newChannel
                   let listenButton :: Event ()
                       listenButton =
                         clickedbut >> (always
                                          (putStrLn "clicked button") >>
                                        listenButton) +>
                         receive death
                   spawnEvent listenButton
                   return (but, syncNoWait (send death ()))),
         newline, newline,
         clipup
           [link "This is a clipup"]
           [leftmargin 10
              [prose "Another line of prose"]]]

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
    finishHTk
