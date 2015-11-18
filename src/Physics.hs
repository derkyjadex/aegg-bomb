module Physics
       (Pos, Vec, Box, Trace(..), canSee, trace, boxAt, boxesIntersect)
       where

type Pos = (Double, Double)
type Vec = (Double, Double)
type Box = (Pos, Pos)

data Trace = Trace Pos Double
             deriving (Show, Eq)

minX :: Box -> Double
minX ((x, _), _) = x

minY :: Box -> Double
minY ((_, y), _) = y

maxX :: Box -> Double
maxX (_, (x, _)) = x

maxY :: Box -> Double
maxY (_, (_, y)) = y

getX :: (Double, Double) -> Double
getX = fst

getY :: (Double, Double) -> Double
getY = snd

epsilon :: Double
epsilon = 1e-10

boxesIntersect :: Box -> Box -> Bool
boxesIntersect a b =
  minX a < maxX b && maxX a > minX b &&
  minY a < maxY b && maxY a > minY b

boxAt :: Box -> Pos -> Box
boxAt box (x,y) = ((minX box + x,minY box + y),(maxX box + x,maxY box + y))

traceToBox :: Pos -> Box -> Vec -> Box -> Box -> Trace -> (Trace, Bool)
traceToBox start (boundsMin,boundsMax) motion sweep (boxMin,boxMax) (Trace end frac) =
  if not $
     boxesIntersect sweep
                    (boxMin,boxMax)
     then (Trace end frac,False)
     else let (endX',endY',frac',changed) = result getX getY end frac
              (endY'',endX'',frac'',changed') =
                result getY getX (endX',endY') frac'
          in (Trace (endX'',endY'') frac'',changed || changed')
  where result dim1 dim2 end frac =
          let end2 = dim2 start + dim2 motion * dimFrac dim1
              min2 = end2 + dim2 boundsMin
              max2 = end2 + dim2 boundsMax
          in if dimFrac dim1 >= frac ||
                dimFrac dim1 < -epsilon ||
                dim1 motion == 0 || max2 < dim2 boxMin || min2 > dim2 boxMax
                then (dim1 end,dim2 end,frac,False)
                else let end1 =
                           if dim1 motion < 0
                              then dim1 boxMax - dim1 boundsMin
                              else dim1 boxMin - dim1 boundsMax
                     in (end1,end2,dimFrac dim1,True)
        dimFrac dim =
          if dim motion < 0
             then (dim boxMax - dim start - dim boundsMin) / dim motion
             else (dim boxMin - dim start - dim boundsMax) / dim motion

trace :: Pos -> Box -> Vec -> [Box] -> Trace
trace (x,y) bounds (dx,dy) boxes =
  let initial = Trace (x + dx,y + dy) 1
  in foldr ((fst .) .
            traceToBox (x,y)
                       bounds
                       (dx,dy)
                       sweep)
           initial
           boxes
  where sweep =
          let minX' = x + minX bounds + min 0 dx
              minY' = y + minY bounds + min 0 dy
              maxX' = x + maxX bounds + max 0 dx
              maxY' = y + maxY bounds + max 0 dy
          in ((minX',minY'),(maxX',maxY'))

canSee :: Pos -> Pos -> [Box] -> Bool
canSee a b boxes = not $ any (lineIntersectsBox (a,b)) boxes

lineIntersectsBox :: (Pos, Pos) -> Box -> Bool
lineIntersectsBox ((x1,y1),(x2,y2)) ((minX,minY),(maxX,maxY))
  | quickOut = False
  | quickIn = True
  | otherwise = intersectsLines
  where quickOut =
          (x1 < minX && x2 < minX) ||
          (x1 > maxX && x2 > maxX) ||
          (y1 < minY && y2 < minY) || (y1 > maxY && y2 > maxY)
        quickIn =
          (x1 >= minX && x1 <= maxX && x2 >= minX && x2 <= maxX) ||
          (y1 >= minY && y1 <= maxY && y2 >= minY && y2 <= maxY) ||
          (x1 >= minX && x1 <= maxX && y1 >= minY && y1 <= maxY) ||
          (x2 >= minX && x2 <= maxX && y2 >= minY && y2 <= maxY)
        yAt x =
          let a = (y2 - y1) / (x2 - x1)
              b = y1 - a * x1
          in a * x + b
        xAt y =
          let a = (x2 - x1) / (y2 - y1)
              b = x1 - a * y1
          in a * y + b
        intersectsLines =
          (xAt minY >= minX && xAt minY <= maxX) ||
          (xAt maxY >= minX && xAt maxY <= maxX) ||
          (yAt minX >= minY && yAt minX <= maxY) ||
          (yAt maxX >= minY && yAt maxX <= maxY)
