-- #####################################################################
--
-- MODULE		: TreeList
-- AUTHOR		: Joel Wright
-- DATE			: 2000
-- VERSION		: Alpha
-- DESCRIPTION		: Implements a tree-list structure needed for the
--			  File Manager
--		  
-- #####################################################################

module TreeListPath (

	Treelistpath(..),
	Treepath,
	
	getNumberElements,
	getNumberOpenElements,
	getNumberElementsIO,
	getNumberOpenElementsIO,
	getFilePathIO,
	getTreePathIO,
	getFolders,

	initTreeList,
	
	changeTree,
	changeTreeIO,
	simpleOpen,
	complexClose,
	simpleClose,

	makeChildTrees,
	swapTree
	
		    ) where
		    
import Directory
import IO

-- #####################################################################
-- The tree-list-path data structure
-- #####################################################################


type Treepath = [Int]

data Treelistpath a b c = Node a Bool Treepath [Treelistpath a Bool Treepath]
	deriving (Show,Eq)
	

-- #####################################################################
-- TreeListPath Functions
-- #####################################################################


initTreeList :: IO (Treelistpath String Bool Treepath)
initTreeList = do
	tree <- makeChildTrees (Node "/" False [] []) "/"
	return tree

makeChildTrees :: Treelistpath String Bool Treepath -> FilePath -> IO (Treelistpath String Bool Treepath)
makeChildTrees (Node a b tp tlps) directory = do
	files <- getDirectoryContents directory
	setCurrentDirectory directory
	folders <- getFolders files
	if (folders == []) then return (Node a b tp tlps)
	 else do
		tree <- addTreePath (Node a True tp (map makeTreeFromFolder folders))
		return tree

makeTreeFromFolder :: String -> Treelistpath String Bool Treepath
makeTreeFromFolder name = (Node name False [] [])

addTreePath :: Treelistpath a Bool Treepath -> IO (Treelistpath a Bool Treepath)
addTreePath (Node a b tp tlps) = do
		tlists <- addChildTreePaths tp [1 .. (length(tlps))] tlps
		return (Node a b tp tlists)

addChildTreePaths :: Treepath -> [Int] -> [Treelistpath a Bool Treepath] -> IO [Treelistpath a Bool Treepath]
addChildTreePaths _ [] _ = return []
addChildTreePaths tp (x:xs) ((Node a b pth tlpths):ns) = do
		nodes <- addChildTreePaths tp xs ns
		return ((Node a b (x:tp) tlpths):nodes)


-- #####################################################################
-- 'Get' Functions
-- #####################################################################


getFolders :: [FilePath] -> IO [FilePath]
getFolders [] = return []
getFolders (f:rem) = if ((f == ".") || (f == "..")) then
		do
		  r <- getFolders rem
		  return r
		else do
			b <- doesDirectoryExist f
			r <- getFolders rem
			return (if b then (f:r) else r)
		
getNumberElements :: Treelistpath a Bool Treepath -> Double
getNumberElements (Node a b c []) = 1
getNumberElements (Node a b c tlps) = 1 + sum(map getNumberElements tlps)

getNumberElementsIO :: Treelistpath a Bool Treepath -> IO Double
getNumberElementsIO tlp = return (getNumberElements tlp)

getNumberOpenElements :: Treelistpath a Bool Treepath -> Double
getNumberOpenElements (Node a b c []) = 1
getNumberOpenElements (Node a b c tlps)
	| (b==True) = 1 + sum(map getNumberOpenElements tlps)
	| otherwise = 1
	
getNumberOpenElementsIO :: Treelistpath a Bool Treepath -> IO Double
getNumberOpenElementsIO tlp = return (getNumberOpenElements tlp)

getTreeListAtIndex :: Treelistpath a Bool Treepath -> Treepath -> Treelistpath a Bool Treepath
getTreeListAtIndex tlp [] = tlp
getTreeListAtIndex (Node a b c []) (x:xs) = error "Path too deep"
getTreeListAtIndex (Node a b c tlps) (x:xs) = getTreeListAtIndex (getTreeFromList x tlps) xs

getTreeFromList :: Int -> [Treelistpath a Bool Treepath] -> Treelistpath a Bool Treepath
getTreeFromList x [tlp]
	| (x==(head(getTreePath tlp))) = tlp
	| otherwise = error "Cannot get treelistpath - path index too high"
getTreeFromList x (tlp:tlps)
	| (x==(head(getTreePath tlp))) = tlp
	| otherwise = getTreeFromList (x-1) tlps

getFilePathNonIO :: Treelistpath String Bool Treepath -> Treepath -> String
getFilePathNonIO (Node a _ _ _) [] = (a++"/")
getFilePathNonIO (Node a _ _ []) (p:ps) = error "cannot reach selected path - getFilePathNonIO error"
getFilePathNonIO (Node a b c (tlp:tlps)) (p:ps)
	| (p == (head(getTreePath tlp))) = a ++ "/" ++ (getFilePathNonIO tlp ps)
	| otherwise = getFilePathNonIO (Node a b c tlps) (p:ps)
	
getFilePathNonIO2 :: Treelistpath String Bool Treepath -> Treepath -> String
getFilePathNonIO2 tlp pth = tail (getFilePathNonIO tlp pth)

getFilePathIO :: Treelistpath String Bool Treepath -> Treepath -> IO String
getFilePathIO tlp pth = return (getFilePathNonIO2 tlp pth)

getTreePath :: Treelistpath a Bool Treepath -> Treepath
getTreePath (Node _ _ pth _) = pth

getTreePathIO :: Treelistpath a Bool Treepath -> IO Treepath
getTreePathIO tlp = return (getTreePath tlp)

getNumberChildren :: Treelistpath a Bool Treepath -> Int
getNumberChildren (Node _ _ _ tlps) = length tlps


-- ##########################
-- Change Tree Functions
-- ##########################


simpleClose :: Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath
simpleClose (Node a True pth tlps) = Node a False pth tlps
simpleClose tlp = tlp

complexClose :: Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath
complexClose (Node a True pth []) = Node a False pth []
complexClose (Node a True pth tlps) = Node a False pth (map complexClose tlps)
complexClose tlp = tlp

simpleOpen :: Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath
simpleOpen (Node a False pth tlps) = Node a True pth tlps
simpleOpen tlp = tlp


-- ##############################
-- Change Tree Mechanism
-- ##############################


changeTree :: Treelistpath a Bool Treepath -> Treepath -> (Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath) -> Treelistpath a Bool Treepath
changeTree tlp [] f = (f tlp) 
changeTree (Node a b pth []) (x:xs) f = error "Cannot reach the indexed tree (changeTree error)"
changeTree (Node a b pth tls) (x:xs) f = Node a b pth (applyChange (x:xs) tls f)

changeTreeIO :: Treelistpath a Bool Treepath -> Treepath -> (Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath) -> IO (Treelistpath a Bool Treepath)
changeTreeIO tree list function = return (changeTree tree list function)

applyChange :: Treepath -> [Treelistpath a Bool Treepath] -> (Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath) -> [Treelistpath a Bool Treepath]
applyChange (x:xs) [] f = error "Cannot reach the indexed tree (applyChange error)"
applyChange (x:xs) (tl:tls) f
	| (x==1) = (changeTree tl xs f) : tls
	| otherwise = tl : (applyChange (x-1:xs) tls f)

swapTree :: Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath -> Treelistpath a Bool Treepath
swapTree insertTree originalTree = insertTree


