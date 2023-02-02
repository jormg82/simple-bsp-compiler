
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


-- The file with de data
type Width     = Int
type Height    = Int
type Wall      = (Int, Int, Int, Int)
data FileWalls = FileWalls Width Height [Wall]


type Map = (G.Polygon, [G.Segment])


-- A BSP tree
-- The nodes contains the partitioner segments,
-- and one leaf contains a convex polygon
data BSPTree = Node G.Segment BSPTree BSPTree
             | Leaf G.Polygon
             deriving Show

-- the main function of the partition
bsp :: Map -> BSPTree
bsp (pol, [])   = Leaf pol
bsp (pol, x:xs) = case G.partitionPolygon pol x of
                    (Just p1, Nothing) -> Leaf p1
                    (Nothing, Just p2) -> Leaf p2
                    (Just p1, Just p2) -> Node x (bsp (p1, xs)) (bsp (p2, xs))
                    (Nothing, Nothing) -> error "impossible!!"


parseFileWalls :: String -> FileWalls
parseFileWalls strin = FileWalls w h walls
  where
    (ws:hs:ss) = lines strin
    w          = read ws :: Int    
    h          = read hs :: Int    
    intsToWall = \[x1, y1, x2, y2] -> (x1, y1, x2, y2)
    walls      = map (intsToWall . map read . words) ss


toBSPFile :: Width -> Height -> BSPTree -> String
toBSPFile w s tree = show w ++ "\n" ++ show s ++ "\n" ++ writeTree tree


writeTree :: BSPTree -> String
writeTree (Leaf pol) = "Leaf " ++ G.writePolygon pol
writeTree (Node seg l r) = "Node " ++ G.writeSegment seg ++ "\n"
                        ++ "Left " ++ writeTree l ++ "\n"
                        ++ "Right " ++ writeTree r
         

-- Walls are strictly inside the
wallToSegment :: Wall -> G.Segment
wallToSegment (x1, y1, x2, y2) = G.visibleSegment (x1', y1') (x2', y2')
  where
    x1' = fromIntegral x1
    y1' = fromIntegral y1
    x2' = fromIntegral x2
    y2' = fromIntegral y2


checkWall :: G.Polygon -> G.Segment -> Bool
checkWall pol segment = G.checkSegment segment && G.checkInside pol segment


wallsToMap :: FileWalls -> Map
wallsToMap (FileWalls w h walls) = (initPol, segs')
  where
    w'      = fromIntegral w
    h'      = fromIntegral h
    s0      = G.visibleSegment (0, 0) (w', 0)
    s1      = G.visibleSegment (w', 0) (w', h')
    s2      = G.visibleSegment (w', h') (h', 0)
    s3      = G.visibleSegment (h', 0) (0, 0)
    initPol = [s0, s1, s2, s3]
    segs    = map (G.enlargeSegment initPol . wallToSegment) walls
    segs'   = catMaybes $ map G.merges $ groupBy G.colinear segs



main :: IO ()
main = do file <- parseFileWalls <$> readFile "walls.txt"
          let FileWalls w h _ = file
          writeFile "bsptree.txt" $ toBSPFile w h $ bsp $ wallsToMap file

