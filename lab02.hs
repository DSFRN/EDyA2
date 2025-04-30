module Lab02 where

{-
   Laboratorio 2
   EDyAII 2022
-}

import Data.List

-- 1) Dada la siguiente definición para representar árboles binarios:

data BTree a = E | Leaf a | Node (BTree a) (BTree a)

-- Definir las siguientes funciones:

-- a) altura, devuelve la altura de un árbol binario.

altura :: BTree a -> Int
altura E          = 0
altura (Leaf a)   = 1
altura (Node l r) = max (1 + (altura l)) (1 + (altura r))

-- b) perfecto, determina si un árbol binario es perfecto (un árbol binario es perfecto si
--   cada nodo tiene 0 o 2 hijos y todas las hojas están a la misma distancia desde la raı́z).

perfecto :: BTree a -> Bool
perfecto E          = False
perfecto (Leaf a)   = True
perfecto (Node l r) = (perfecto l && perfecto r) && (altura l == altura r)
-- (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 8)))

-- c) inorder, dado un árbol binario, construye una lista con el recorrido inorder del mismo.

inorder :: BTree a -> [a]
inorder E          = []
inorder (Leaf a)   = [a]
inorder (Node l r) = inorder l ++ inorder r
-- (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Node (Leaf 8) (Leaf 1))))

-- 2) Dada las siguientes representaciones de árboles generales y de árboles binarios (con información en los nodos):

data GTree a = EG | NodeG a [GTree a]

data BinTree a = EB | NodeB (BinTree a) a (BinTree a)

{- Definir una función g2bt que dado un árbol nos devuelva un árbol binario de la siguiente manera:
   la función g2bt reemplaza cada nodo n del árbol general (NodeG) por un nodo n' del árbol binario (NodeB ), donde
   el hijo izquierdo de n' representa el hijo más izquierdo de n, y el hijo derecho de n' representa al hermano derecho
   de n, si existiese (observar que de esta forma, el hijo derecho de la raı́z es siempre vacı́o).
   
   
   Por ejemplo, sea t: 
       
                    A 
                 / | | \
                B  C D  E
               /|\     / \
              F G H   I   J
             /\       |
            K  L      M    
   
   g2bt t =
         
                  A
                 / 
                B 
               / \
              F   C 
             / \   \
            K   G   D
             \   \   \
              L   H   E
                     /
                    I
                   / \
                  M   J  
-}

g2bt :: GTree a -> BinTree a
g2bt t = g2btAux t []

g2btAux :: GTree a -> [GTree a] -> BinTree a
g2btAux EG []                   = EB
g2btAux (NodeG a []) []         = NodeB EB a EB
g2btAux (NodeG a []) (x:xs)     = NodeB EB a (g2btAux x xs)
g2btAux (NodeG a (y:ys)) []     = NodeB (g2btAux y ys) a EB
g2btAux (NodeG a (y:ys)) (x:xs) = NodeB (g2btAux y ys) a (g2btAux x xs)

inorderB :: BinTree a -> [a]
inorderB EB            = []
inorderB (NodeB l a r) = (inorderB l) ++ [a] ++ (inorderB r) 


-- 3) Utilizando el tipo de árboles binarios definido en el ejercicio anterior, definir las siguientes funciones: 
{-
   a) dcn, que dado un árbol devuelva la lista de los elementos que se encuentran en el nivel más profundo 
      que contenga la máxima cantidad de elementos posibles. Por ejemplo, sea t:
            1
          /   \
         2     3
          \   / \
           4 5   6
                             
      dcn t = [2, 3], ya que en el primer nivel hay un elemento, en el segundo 2 siendo este número la máxima
      cantidad de elementos posibles para este nivel y en el nivel tercer hay 3 elementos siendo la cantidad máxima 4.
-}

maxProfComp :: BinTree a -> Int
maxProfComp EB             = 0
maxProfComp (NodeB EB _ _) = 1
maxProfComp (NodeB _ _ EB) = 1
maxProfComp (NodeB l a r)  = min (1 + maxProfComp l) (1 + maxProfComp r)

dcn :: BinTree a -> [a]
dcn t = dcnAux t 1 (maxProfComp t)

dcnAux :: BinTree a -> Int -> Int -> [a]
dcnAux EB _ _ = []
dcnAux (NodeB l a r) n m = if (n == m)
                           then [a]
                           else dcnAux l (n+1) m ++ dcnAux r (n+1) m
-- (NodeB (NodeB EB 2 (NodeB EB 4 EB)) 1 (NodeB (NodeB EB 5 EB) 3 (NodeB EB 6 EB)))

{- b) maxn, que dado un árbol devuelva la profundidad del nivel completo
      más profundo. Por ejemplo, maxn t = 2
-}

maxn :: BinTree a -> Int
maxn t = maxProfComp t

{- c) podar, que elimine todas las ramas necesarias para transformar
      el árbol en un árbol completo con la máxima altura posible. 
      Por ejemplo,
         podar t = NodeB (NodeB EB 2 EB) 1 (NodeB EB 3 EB)
-}

podar :: BinTree a -> BinTree a
podar t = podarAux t 1 (maxn t)

podarAux :: BinTree a -> Int -> Int -> BinTree a
podarAux EB _ _ = EB
podarAux (NodeB l a r) n m = if (n == m)
                             then (NodeB EB a EB)
                             else (NodeB (podarAux l (n+1) m) a (podarAux r (n+1) m))

