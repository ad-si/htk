module Parser (readStructure, readStructureStr) where

import Graph

-- reads a file and parses it
readStructure :: FilePath -> IO Graph
readStructure path = do file <- (readFile path)
			return (parse file)

readStructureStr :: String -> IO Graph
readStructureStr path = do file <- (readFile path)
                           return (parse file)

-- parses a text
parse = parseLines.reverse.(map splitLine).splitIntoLines

-- splits a text (ie a single string) into its lines
splitIntoLines :: String -> [String]
splitIntoLines [] = []
splitIntoLines st = line:(splitIntoLines(dropWhile (=='\n') (drop (length line) st) ))
			where
			line = takeWhile (/='\n') st

-- splits a line into words by the delimiters contained in "tokens"
splitLine :: String -> [String]
splitLine [] = []
splitLine st = part:(splitLine (dropWhile (elemR tokens) (drop (length(part)+1) st)))
	where
	part = takeWhile (notElemR tokens) st

-- tries to add a node for each line of 2 strings
-- and an edge for each line of 7 strings.
-- lines that differ from these standard lengths are ignored
parseLines :: [[String]] -> Graph
parseLines [] = newGraph
parseLines (x:xs) = case (length x) of
	2 -> addNodeStr (parseLines xs) (head x) (last x)
	7 -> addEdgeStr (parseLines xs) (head x) (x!!1) (x!!3) (x!!4) (x!!5) (last x)
	_ -> parseLines xs

-- notElem and elem with reversed order of arguments
notElemR:: Eq a => [a] -> a -> Bool
notElemR ls e = notElem e ls

elemR :: Eq a => [a] -> a -> Bool
elemR ls e = elem e ls

-- list of the tokens
tokens = [' ' , '[' , ']' , ',']