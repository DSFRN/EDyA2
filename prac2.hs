-- :l (load)    archivo.hs      (Carga un archivo .hs)
-- :r (reload)                  (Carga el último archivo cargado)
-- :i (info)    (++)            (Info sobre algo)

import Data.List

-- 1] --------------------------------------------------------------------------------------
-- a)
not b = case b of
    True -> False
    False -> True

-- b)
init2 :: [a] -> [a]
init2 [x]      = []
init2 (x:xs)   = x : init2 xs
init2 []       = error "empty list"

-- c)
length2 :: Num a => [a] -> Int
length2 []      = 0
length2 (_:l)   = 1 + length2 l

-- d)
list123 = 1 : 2 : 3 : []

-- e)
(++!) :: [a] -> [a] -> [a]
[]       ++! ys = ys
(x:xs)   ++! ys = x : xs ++! ys

-- f)
addToTail :: Num a => a -> [a] -> [a]
addToTail x xs = map (+ x) (tail xs)

-- g)
listmin :: Ord a => [a] -> a
listmin xs = head (sort xs)

-- h)
smap :: (a -> b) -> [a] -> [b]
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

-- 2] --------------------------------------------------------------------------------------
apply :: (a -> b) -> a -> b
apply f x = f x

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _     _    = False

-- 3] --------------------------------------------------------------------------------------
esBisiesto :: Int -> Bool
esBisiesto n = (mod n 4 == 0) && (mod n 100 /= 0 || mod n 400 == 0)