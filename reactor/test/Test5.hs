module Main (
        main    
        ) where


import MatchPS
import PackedString
import Concurrency
import Debug(debug)

main = do {
        rest1 <- doMatch "^*HELLO*$\n" "HELLO\nWORLD" "m";
        rest2 <- doMatch "\n" rest1 "m";
        rest3 <- doMatch "^*WO" rest2 "m";
        block;
        return ()       
        } 

matchTry :: String -> String -> [Char] -> (String,Maybe REmatch)
matchTry pattern message flags =        
        case rem of
                Nothing -> (message,Nothing)
                rem @ (Just r) -> (unpackPS (getAfterMatch r msg),rem)

        where   ptn = packString pattern
                msg = packString message
                rem = matchPS ptn msg flags


doMatch :: String -> String -> [Char] -> IO String
doMatch pattern message flags = do {
        putStr ("--------------------------------------------------\n");
        putStr ("Pattern = " ++ show pattern ++ "\n");
        putStr ("Message = " ++ show message ++ "\n");
        putStr ("Flags = " ++ show flags ++ "\n");
        case matchTry pattern message flags of
                (_,Nothing) -> do {
                        putStr ("!!! NO MATCH FOUND !!!\n");
                        return message
                        }
                (new,rem @ (Just r)) -> do {
                        putStr ("Rest = " ++ show new ++ "\n");
                        return new
                        }
        
}
