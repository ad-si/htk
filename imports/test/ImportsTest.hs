{- This module constructs a dummy file structure for testing the Imports
   module. -}
module ImportsTest where

import System.IO.Unsafe
import Data.FiniteMap

import Monad
import Maybe

import Broadcaster
import AtomString
import Sources
import ExtendedPrelude
import Sink

import EntityNames
import FolderStructure
import Imports


-- -----------------------------------------------------------------------
-- Data for each node
-- -----------------------------------------------------------------------

data N = N {
   n :: Int,
   c :: [(EntityName,Int)], -- contents
   i :: Maybe (SimpleBroadcaster ImportCommands),
   p :: Maybe (Int,EntityName) -- parent
   }

type NPre = (Int,[(String,Int)],Maybe [ImportCommand],Maybe (String,Int))

mkN :: NPre -> IO N
mkN (n,cPre,iPre,pPre) =
   do
      let
         c = map
            (\ (str,i) -> (EntityName str,i))
            cPre

      i <- case iPre of
         Nothing -> return Nothing
         Just ics ->
            do
               broadcaster <- newSimpleBroadcaster (ImportCommands ics)
               return (Just broadcaster)
      let
         p = fmap
            (\ (s,i) -> (i,EntityName s))
            pPre
      return (N n c i p)

ns :: [N]
ns = unsafePerformIO (mapM mkN nPre)
{-# NOINLINE ns #-}

nPre :: [NPre]
nPre = [
   (1,
      [("A",2),
      ("B",5),
      ("C",8)
      ],
      Nothing,
      Nothing
      ),
   (2,
      [("a",3),
       ("b",4)
       ],
      Just [
--         Import [] (fromString "Current")
         PathAlias (fromString "BB") (fromString "Parent.B"),
         Import [Global] (fromString "BB")
         ],
      Just ("A",1)
      ),
   (5,
      [("c",6),
       ("d",7)
       ],
      Just [
         Import [Local] (fromString "Parent.A")
         ],
      Just ("B",1)
      ),
   (8,
      [("e",9)
       ],
      Just [
--         Import [] (fromString "Parent.A"),
--         Import [Qualified] (fromString "Parent.B")
         ],
      Just ("C",1)
      ),
   (3,
      [],
      Nothing,
      Just ("a",2)
      ),
   (4,
      [],
      Nothing,
      Just ("b",2)
      ),
   (6,
      [],
      Nothing,
      Just ("c",5)
      ),
   (7,
      [],
      Nothing,
      Just ("d",5)
      ),
   (9,
      [],
      Nothing,
      Just ("e",8)
      )
   ]

-- -----------------------------------------------------------------------
-- Constructing the FolderStructure
-- -----------------------------------------------------------------------


folderStructure :: FolderStructure Int
folderStructure = FolderStructure {
   root = 1,
   getContentsSource = (return . staticSimpleSource . listToFM . c . toN),
   getImportCommands = (\ i1 ->
      return (fmap
         (\ broadcaster -> toSimpleSource broadcaster)
         (i (toN i1))
         )
      ),
   parent = (return . staticSimpleSource . p . toN)
   }
    


toN :: Int -> N
toN i =
   let
      nOpt = findJust
         (\ n0 -> if n n0 == i then Just n0 else Nothing) 
         ns
   in
      fromMaybe
         (error ("toN " ++ show i))
         nOpt

-- -----------------------------------------------------------------------
-- Running the test
-- -----------------------------------------------------------------------

test1 =
   do
      monitor "A" "a"
--      monitor "B" "a"
--      monitor "C" "b"

monitor :: String -> String -> IO ()
monitor mod name =
   do
      modNode <- getNode mod
      source <- lookupNode importsState modNode (fromString name)
      let
         disp :: LookupResult Int -> IO ()
         disp lr = 
            putStrLn (mod ++ ":" ++ name ++ " "
               ++ (case lr of
                  Found i -> show i
                  NotFound -> "NotFound"
                  Error -> "Error"
                  )
               )

      lookedUp <- addNewAction 
         source
         (\ lr -> 
            do
               disp lr
               return True
            )

      disp lookedUp      
      
getNode :: String -> IO Int
getNode str =
   do
      source <- lookupFullName folderStructure 1 (fromString str)
      nodeOpt <- readContents source
      return (fromMaybe
         (error (str ++ " not found"))
         nodeOpt
         )

importsState :: ImportsState Int
importsState = unsafePerformIO (newImportsState folderStructure putStrLn)
{-# NOINLINE importsState #-}

