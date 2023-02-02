
module Geometry where

import Data.Foldable(foldlM)
import Data.Maybe(catMaybes)

-- A point in the plane
type Point = (Float, Float)

-- A vector in the plane
type Vector = (Float, Float)

-- A line in the plane
data Line = Line Point Vector
            deriving Show

-- Indicates whether a fragment of segment is visible or not
type Visible = Bool

data Segment = Segment [Point] [Visible]
               deriving Show

-- A convex polygon is a set of segments
type Polygon = [Segment]


-- Line constructor
line :: Point -> Vector -> Line
line = Line


segment :: Bool -> Point -> Point -> Segment
segment b p1 p2 = Segment [p1, p2] [b]

visibleSegment :: Point -> Point -> Segment
visibleSegment = segment True

noVisibleSegment :: Point -> Point -> Segment
noVisibleSegment = segment False

writeSegment :: Segment -> String
writeSegment = undefined

writePolygon :: Polygon -> String
writePolygon = undefined


-- Merge according to the first segment direction
merge :: Segment -> Segment -> Maybe Segment
merge = undefined


merges :: [Segment] -> Maybe Segment
merges []     = Nothing
merges (s:ss) = foldlM merge s ss

-- Enlarges a segment inside (strictly except maybe the ends)
-- a convex polygon
enlargeSegment :: Polygon -> Segment -> Segment
enlargeSegment ss segment = segment'
  where
    line = toLine segment
    [p1, p2] = catMaybes $ map (intersectLine line) ss
    Just segment' = merge segment (noVisibleSegment p1 p2)
 

-- Checks that the segment is stricty inside
-- the polygon (maybe except the ends)
checkInside :: Polygon -> Segment -> Bool
checkInside = undefined

-- Checks segment well-formedness
checkSegment :: Segment -> Bool
checkSegment = undefined


class Geops a where
  containsPoint :: Point -> a -> Bool
  parallel :: a -> a -> Bool
  colinear :: a -> a -> Bool
  toLine :: a -> Line
  intersectLine :: Line -> a -> Maybe Point
  intersectSegment :: Segment -> a -> Maybe Point
  partitionSegment :: Segment -> a -> (Maybe Segment, Maybe Segment)
  partitionPolygon :: Polygon -> a -> (Maybe Polygon, Maybe Polygon)

instance Geops Line where
  containsPoint = undefined
  parallel = undefined
  colinear = undefined
  toLine = id
  intersectLine = undefined
  intersectSegment = undefined
  partitionSegment = undefined
  partitionPolygon = undefined

instance Geops Segment where
  containsPoint = undefined
  parallel = undefined
  colinear = undefined
  toLine = undefined
  intersectLine = undefined
  intersectSegment = undefined
  partitionSegment = undefined
  partitionPolygon = undefined

