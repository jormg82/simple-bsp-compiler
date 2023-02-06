
module Geometry where

import Data.Foldable(foldlM)
import Data.List(intercalate)
import Data.Maybe(catMaybes, isJust)



-- A point in the plane
type Point = (Float, Float)

-- A vector in the plane
type Vector = (Float, Float)

type Wall = (Point, Vector)

-- A facade is a set of colinear wall segments
-- with de same direction
type Facade = [Wall]


-- Wall constructors
wall :: Point -> Vector -> Wall
wall p v = (p, v)


-- Write functions
writePoint :: Point -> String
writePoint (x, y) = show x ++ " " ++ show y

writeVector :: Vector -> String
writeVector = writePoint

writeWall :: Wall -> String
writeWall (p, v) = writePoint p ++ " " ++ writeVector v

writeFacade :: Facade -> String
writeFacade = intercalate " " . map writeWall


-- Basic operations between points and vectors
nullVector :: Vector -> Bool
nullVector (u1, u2) = u1 == 0 && u2 == 0

prod :: Vector -> Vector -> Float
prod (u1, u2) (v1, v2) = u1*v1+u2*v2

numProd :: Float -> Vector -> Vector
numProd x (u1, u2) = (x*u1, x*u2)

add :: Vector -> Vector -> Vector
add (u1, u2) (v1, v2) = (u1+v1, u2+v2)

diff :: Vector -> Vector -> Vector
diff (u1, u2) (v1, v2) = (u1-v1, u2-v2)

neg :: Vector -> Vector
neg (u1, u2) = ((-u1), (-u2))

-- Checks if two vectors are lineary dependants
linearyIndep :: Vector -> Vector -> Bool
linearyIndep (u1, u2) (v1, v2) = u1*v2-u2*v1 /= 0


-- Checks if three points are colinear
colinearPoints :: Point -> Point -> Point -> Bool
colinearPoints p1 p2 p3 = not $ linearyIndep (diff p2 p1) (diff p3 p1)


colinearPList :: [Point] -> Bool
colinearPList []         = True
colinearPList [_]        = True
colinearPList (p1:p2:ps) = all (colinearPoints p1 p2) ps


-- apply direction of the first wall to the second,
-- where both walls are colinear
applyDirection :: Wall -> Wall -> Wall
applyDirection (p, u) w@(q, v)
  | signum (fst u) == signum (fst v) = w
  | otherwise = negWall w


-- ESTA
-- Eliminates redundance in the structure of a segment
normalize :: Facade -> Facade
normalize (w:ws) = w:map (applyDirection w) ws


-- ESTA
colinearWalls :: Wall -> Wall -> Bool
colinearWalls (p, u) (q, v) = colinearPList [p, add p u, q, add q v]


negWall :: Wall -> Wall
negWall (p, u) = (add p u, neg u)


cmpPWall :: Point -> Wall -> Ordering
cmpPWall (p1, p2) ((q1, q2), (v1, v2)) = compare 0 ((p1-q1)*v2 - (p2-q2)*v1)


partitionWall :: Wall -> Wall -> (Maybe Wall, Maybe Wall)
partitionWall w@(p, u) w'@(q, v) =
  case (cmpPWall q w, cmpPWall (add q v) w) of
    (EQ, EQ) -> error "Partitioning colinear walls"
    (EQ, LT) -> (Just w', Nothing)
    (LT, EQ) -> (Just w', Nothing)
    (LT, LT) -> (Just w', Nothing)
    (EQ, GT) -> (Nothing, Just w')
    (GT, EQ) -> (Nothing, Just w')
    (GT, GT) -> (Nothing, Just w')
    (LT, GT) -> (Just w1, Just w2)
    (GT, LT) -> (Just w2, Just w1)
  where
    t   = cuttingParam w w'
    v'  = numProd t v
    v'' = numProd (1-t) v
    w1  = wall q v'
    w2  = wall (add q v') v''


-- Cramer rule
cuttingParam :: Wall -> Wall -> Float
cuttingParam ((p1, p2), (u1, u2)) ((q1, q2), (v1, v2)) =
  ((q1-p1)*u2 - (q2-p2)*u1) / (u1*v2 - u2*v1)


partitionFacade :: Facade -> Facade -> (Maybe Facade, Maybe Facade)
partitionFacade (w:_) = both (lsToMaybe . catMaybes) . unzip
                      . map (partitionWall w)


partitionFacades :: Facade -> [Facade] -> ([Facade], [Facade])
partitionFacades f = both catMaybes . unzip . map (partitionFacade f)


both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)


lsToMaybe :: [a] -> Maybe [a]
lsToMaybe []       = Nothing
lsToMaybe xs@(_:_) = Just xs

