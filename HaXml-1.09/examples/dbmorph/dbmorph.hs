module Main where

import Prelude hiding (catch)
import System.Environment        (getArgs)
import System.IO                 (readFile)
import Control.Exception         (catch)
import Data.Maybe                (fromJust)
import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.Parse      (xmlParse)
import Text.XML.HaXml.Pretty     (document)
import Text.PrettyPrint.HughesPJ (render)
import Simple
--import SimpleDocbookDTD

main :: IO()
main
    = do args <- getArgs
         mapM process args
         return ()

process :: FilePath -> IO ()
process path
    = catch (do putStrLn $ "Processing " ++ path ++ " ... "
                inp <- readFile path
                let raw = xmlParse path inp
                    doc = fromJust (readXml inp) :: Article
                (putStrLn . render . document) raw
                (putStrLn . show) doc
                fWriteXml (path ++ "-out") doc
                putStrLn "****OK")
            (\e -> putStr $ show e)
