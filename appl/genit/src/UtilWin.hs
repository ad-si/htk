
module UtilWin(

  display :: String-> -- title
             MarkupText-> -- text to be displayed
             IO ()

  error :: String-> IO ()
  warning :: String-> IO ()
  info  :: String-> IO ()

  enterLine :: String-> -- title
               String-> -- prompt
               String-> -- default
               IO String

  enterText :: ...

) where
