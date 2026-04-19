module PoinTree where
import Data.List

data NdTree p = Empty | Node (NdTree p) p (NdTree p) Int 
                deriving (Eq, Ord, Show)

-- (1)
class Punto p where
  dimension :: p -> Int
  coord :: Int -> p -> Double
  dist :: p -> p -> Double

-- a)
  dist p q = sqrt (dist' p q (dimension p - 1)) where
              dist' p q 0 = (coord 0 p - coord 0 q)^2
              dist' p q i = (coord i p - coord i q)^2 + dist' p q (i-1)

-- b)
newtype Punto2d = P2d (Double, Double) deriving (Eq, Show)
newtype Punto3d = P3d (Double, Double, Double) deriving (Eq, Show)

instance Punto Punto2d where
  dimension _ = 2
  coord i (P2d (x,y)) | (i == 0) = x
                      | (i == 1) = y
                      | otherwise = error "coord: indice fuera de rango."

instance Punto Punto3d where
  dimension _ = 3
  coord i (P3d (x,y,z)) | (i == 0) = x
                        | (i == 1) = y
                        | (i == 2) = z
                        | otherwise = error "coord: indice fuera de rango."


-- (2)
fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList ps = fromList' ps 0
  where
    fromList' [] _     = Empty
    fromList' ps level =
      let
        n      = dimension (head ps)
        axis   = mod level n
        sorted = sortBy (\p q -> compare (coord axis p) (coord axis q)) ps
        mid    = div (length sorted) 2
        median = sorted !! mid
        left   = take mid sorted
        right  = drop (mid+1) sorted
      in
        Node (fromList' left (level+1)) median (fromList' right (level+1)) axis

-- Test:
-- fromList [P2d (2.0,3.0), P2d (5.0,4.0), P2d (9.0,6.0), P2d (4.0,7.0), P2d (8.0,1.0), P2d (7.0,2.0)]


-- (3)
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p Empty = Node Empty p Empty 0
insertar p (Node left q right axis) =
  case compare (coord axis p) (coord axis q) of
  GT -> case right of
        Empty -> Node left q leaf axis
        _     -> Node left q (insertar p right) axis
  _  -> case left of
        Empty -> Node leaf q right axis
        _     -> Node (insertar p left) q right axis
  where
    leaf = Node Empty p Empty (mod (axis+1) (dimension p))

-- Test:
--   tree = fromList [P2d (7.0,2.0), P2d (5.0,4.0), P2d (9.0,6.0)]
-- insertar (P2d (2.0,3.0)) tree


-- (4)
minByAxis :: Punto p => Int -> NdTree p -> p
minByAxis axis (Node Empty p Empty _) = p
minByAxis axis (Node left  p Empty _) = minPoint axis p (minByAxis axis left)
minByAxis axis (Node Empty p right _) = minPoint axis p (minByAxis axis right)
minByAxis axis (Node left  p right _) = minPoint axis p (minPoint axis (minByAxis axis left) (minByAxis axis right))

minPoint :: Punto p => Int -> p -> p -> p
minPoint axis p q = if (coord axis p) <= (coord axis q) then p else q

maxByAxis :: Punto p => Int -> NdTree p -> p
maxByAxis axis (Node Empty p Empty _) = p
maxByAxis axis (Node left  p Empty _) = maxPoint axis p (maxByAxis axis left)
maxByAxis axis (Node Empty p right _) = maxPoint axis p (maxByAxis axis right)
maxByAxis axis (Node left  p right _) = maxPoint axis p (maxPoint axis (maxByAxis axis left) (maxByAxis axis right))

maxPoint :: Punto p => Int -> p -> p -> p
maxPoint axis p q = if (coord axis p) >= (coord axis q) then p else q


eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar p Empty = Empty
eliminar p t@(Node left q right axis) =
  if (p == q)
  then case t of
       Node Empty q Empty axis -> Empty
       Node left  q Empty axis -> let x = maxByAxis axis left
                                  in  Node (eliminar x left) x Empty axis
       Node left  q right axis -> let x = minByAxis axis right
                                  in  Node left x (eliminar x right) axis
 else case compare (coord axis p) (coord axis q) of
       GT -> Node left q (eliminar p right) axis
       _  -> Node (eliminar p left) q right axis

-- Test:
--   tree = fromList [P2d (1.0,1.0), P2d (2.0,7.0), P2d (3.0,4.0), P2d (5.0,5.0), P2d (7.0,9.0), P2d (8.0,2.0), P2d (9.0,1.0)]
-- eliminar (P2d (5.0,5.0)) tree


-- (5)
type Rect = (Punto2d, Punto2d)

-- a)
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (p1,p2)) ((P2d (xr1, yr1)),(P2d (xr2, yr2))) =
  let xmin = min xr1 xr2
      xmax = max xr1 xr2
      ymin = min yr1 yr2
      ymax = max yr1 yr2
  in 
      p1 >= xmin && p1 <= xmax && p2 >= ymin && p2 <= ymax

-- b)
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node left p right axis) rect@((P2d (xr1, yr1)), (P2d (xr2, yr2))) =
  let search = if axis == 0
               then (if xmin <= value then leftSearch else []) ++
                    (if xmax >  value then rightSearch else [])
               else (if ymin <= value then leftSearch else []) ++
                    (if ymax >  value then rightSearch else [])
  in inside ++ search
  where
    inside = if inRegion p rect then [p] else []
    leftSearch  = ortogonalSearch left rect
    rightSearch = ortogonalSearch right rect
    xmin  = min xr1 xr2
    xmax  = max xr1 xr2
    ymin  = min yr1 yr2
    ymax  = max yr1 yr2
    value = coord axis p

-- Test:
--   tree = fromList [P2d (7.0,2.0), P2d (5.0,4.0), P2d (9.0,6.0), P2d (4.0,7.0), P2d (8.0,1.0), P2d (2.0,3.0), P2d (10.0,8.0)]
--   rect = (P2d (3.0,2.0), P2d (8.0,7.0))
-- ortogonalSearch tree rect
