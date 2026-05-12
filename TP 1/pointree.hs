module PoinTree where
import Data.List

-- Trabajo Práctico EDyA II
-- Grupo: Franco Di Santis

data NdTree p = Empty | Node (NdTree p) p (NdTree p) Int 
                deriving (Eq, Ord, Show)

-- (1) ------------------------------------------------------------------------------------------//

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


-- (2) ------------------------------------------------------------------------------------------//

-- Construye un NdTree balanceado a partir de una lista de puntos.
-- El algoritmo selecciona en cada nivel el eje = level 'mod' n,
-- ordena los puntos según ese eje, elige la mediana como raíz y
-- recursa sobre ambas mitades restantes.
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


-- (3) ------------------------------------------------------------------------------------------//

-- Inserta un punto en un NdTree manteniendo su invariante.
-- Desciende comparando la coordenada del eje correspondiente
-- de forma similar a un BST. Al llegar a un nodo vacío,
-- inserta el punto como una hoja.
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p Empty = Node Empty p Empty 0
insertar p (Node left q right axis) =
  case compare (coord axis p) (coord axis q) of
  -- El punto va a la derecha.
  GT -> case right of
        Empty -> Node left q leaf axis
        _     -> Node left q (insertar p right) axis
  -- El punto va a la izquierda.
  _  -> case left of
        Empty -> Node leaf q right axis
        _     -> Node (insertar p left) q right axis
  where
    leaf = Node Empty p Empty (mod (axis+1) (dimension p))

-- Test:
--   tree = fromList [P2d (7.0,2.0), P2d (5.0,4.0), P2d (9.0,6.0)]
-- insertar (P2d (2.0,3.0)) tree


-- (4) ------------------------------------------------------------------------------------------//

-- Devuelve el punto con menor valor respecto a un eje dado dentro del subárbol.
minByAxis :: Punto p => Int -> NdTree p -> p
minByAxis axis (Node Empty p Empty _) = p
minByAxis axis (Node left  p Empty _) = minPoint axis p (minByAxis axis left)
minByAxis axis (Node Empty p right _) = minPoint axis p (minByAxis axis right)
minByAxis axis (Node left  p right _) = minPoint axis p (minPoint axis (minByAxis axis left) (minByAxis axis right))

-- Compara dos puntos según una coordenada y devuelve el de menor valor.
minPoint :: Punto p => Int -> p -> p -> p
minPoint axis p q = if (coord axis p) <= (coord axis q) then p else q

-- Devuelve el punto con mayor valor respecto a un eje dado dentro del subárbol.
maxByAxis :: Punto p => Int -> NdTree p -> p
maxByAxis axis (Node Empty p Empty _) = p
maxByAxis axis (Node left  p Empty _) = maxPoint axis p (maxByAxis axis left)
maxByAxis axis (Node Empty p right _) = maxPoint axis p (maxByAxis axis right)
maxByAxis axis (Node left  p right _) = maxPoint axis p (maxPoint axis (maxByAxis axis left) (maxByAxis axis right))

-- Compara dos puntos según una coordenada y devuelve el de mayor valor.
maxPoint :: Punto p => Int -> p -> p -> p
maxPoint axis p q = if (coord axis p) >= (coord axis q) then p else q

-- Elimina un punto del NdTree manteniendo su invariante.
--  >  Si es una hoja, se elimina directamente.
--  >  Si tiene subárbol derecho, lo reemplaza con el mínimo del subárbol derecho
--     (según su eje), y luego elimina ese mínimo.
--  >  Si tiene subárbol izquierdo, lo reemplaza con el máximo del subárbol izquierdo
--     (según su eje), y luego elimina ese máximo.
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


-- (5) ------------------------------------------------------------------------------------------//

type Rect = (Punto2d, Punto2d)

-- a)
-- Determina si un punto 2D está dentro de un rectángulo.
inRegion :: Punto2d -> Rect -> Bool
inRegion (P2d (p1,p2)) ((P2d (xr1, yr1)),(P2d (xr2, yr2))) =
  let xmin = min xr1 xr2
      xmax = max xr1 xr2
      ymin = min yr1 yr2
      ymax = max yr1 yr2
  in 
      p1 >= xmin && p1 <= xmax && p2 >= ymin && p2 <= ymax

-- b)
-- Devuelve todos los puntos del NdTree que están dentro del rectángulo dado.
-- Aprovecha la estructura del árbol para podar ramas enteras:
--   Si el valor del eje de un nodo cae fuera del rango del rectángulo en ese eje,
--  no es necesario explorar el subárbol correspondiente, reduciendo así el costo.
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch (Node left p right axis) rect@((P2d (xr1, yr1)), (P2d (xr2, yr2))) =
  let search = if axis == 0
               -- Caso eje X:
               then (if xmin <= value then leftSearch else []) ++
                    (if xmax >  value then rightSearch else [])
               -- Caso eje Y:
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

