
module Main where

import qualified Geometry as G

import Data.List(groupBy)
import Data.Maybe(catMaybes)


{-- The file walls.txt has the following format,
-   where width, height and all the coordenates
-   are integers:
-
-   width
-   height
-   fromx fromy tox toy
-   ...
-   ...
--}


-- The file with de map data
type Width     = Int
type Height    = Int
type FileWall  = (Int, Int, Int, Int)

data FileWalls = FileWalls Width Height [FileWall]
                 deriving Show

-- A BSP tree
-- The nodes contains the colinear partitioner walls
data BSPTree = Node G.Facade BSPTree BSPTree
             | Leaf
             deriving Show
 

-- the main partition function
bsp :: [G.Facade] -> BSPTree
bsp []     = Leaf
bsp [f]    = Node f Leaf Leaf
bsp (f:fs) = Node f (bsp fs1) (bsp fs2)
             where
               (fs1, fs2) = G.partitionFacades f fs


parseFileWalls :: String -> FileWalls
parseFileWalls strin = FileWalls w h fwalls
  where
    (ws:hs:ss) = lines strin
    w          = read ws :: Int    
    h          = read hs :: Int    
    fwalls     = map (toquad . map read . words) ss
    toquad [x, y, z, t] = (x, y, z, t)


writeBSPTree :: BSPTree -> String
writeBSPTree Leaf         = "Leaf\n"
writeBSPTree (Node f l r) = "Node\n" ++ G.writeFacade f ++ "\n"
                         ++ "Left\n" ++ writeBSPTree l
                         ++ "Right\n" ++ writeBSPTree r
         

-- the walls strictly inside the limits, except maybe
-- one end
checkPos :: Width -> Height -> FileWall -> Bool
checkPos w h (x1, y1, x2, y2) = 0<=x1 && x1<w && 0<=x2 && x2<w
                             && 0<=y1 && y1<h && 0<=y2 && y2<h


fileWallToGWall :: FileWall -> G.Wall
fileWallToGWall (x1, y1, x2, y2) = (p1, G.diff p2 p1)
  where
    p1 = (fromIntegral x1, fromIntegral y1)
    p2 = (fromIntegral x2, fromIntegral y2)


fileWallsToFacades :: FileWalls -> [G.Facade]
fileWallsToFacades (FileWalls w h fwalls)
  | w>0 && h>0 && all (checkPos w h) fwalls = facades
  | otherwise = error "Bad walls file"
  where
    fw0     = (0, 0, w-1, 0)
    fw1     = (w-1, 0, w-1, h-1)
    fw2     = (w-1, h-1, 0, h-1)
    fw3     = (0, h-1, 0, 0)
    fwalls' = fwalls ++ [fw0, fw1, fw2, fw3]  
    walls   = map fileWallToGWall fwalls'
    facades = map G.normalize (groupBy G.colinearWalls walls)


main :: IO ()
main = do
  file <- parseFileWalls <$> readFile "walls.txt"
  let FileWalls w h _ = file
      facades = fileWallsToFacades file
      tree    = bsp facades
      outstr  = show w ++ "\n" ++ show h ++ "\n" ++ writeBSPTree tree
  writeFile "bsptree.txt" outstr

