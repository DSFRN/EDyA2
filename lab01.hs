-- :l (load)    archivo.hs      (Carga un archivo .hs)
-- :r (reload)                  (Carga el último archivo cargado)
-- :i (info)    (++)            (Info sobre algo)

import Data.List
import Data.Char

-- 1] --------------------------------------------------------------------------------------
-- a)
not1 b = case b of
     True -> False
     False -> True

-- b)
init2 :: [a] -> [a]
init2 [x]      = []
init2 (x:xs)   = x : init2 xs
init2 []       = error "empty list"

-- c)
length2 :: Num b => [a] -> b
length2 []      = 0
length2 (_:l)   = 1 + length2 l

-- d)
list123 = 1 : 2 : 3 : []

-- e)
(++!) :: [a] -> [a] -> [a]
[]      ++! ys   = ys
(x:xs)  ++! ys   = x : xs ++! ys

-- f)
addToTail :: Num a => a -> [a] -> [a]
addToTail x xs = map (+ x) (tail xs)

-- g)
listmin :: Ord a => [a] -> a
listmin xs = head (sort xs)

-- h)
smap :: (a -> b) -> [a] -> [b]
smap f []       = []
smap f [x]      = [f x]
smap f (x:xs)   = f x : smap f xs

-- 2] --------------------------------------------------------------------------------------
five :: a -> Int
five x = 5

apply :: (a -> b) -> a -> b
apply f x = f x

ident :: Num a => a -> a
ident a = a

first :: (a,b) -> a
first (a,_) = a

--derive :: (Num a, Integral b) => (a -> b) -> a -> b


sign :: (Ord a, Num a) => a -> Int
sign a | a > 0       = 1
       | a == 0      = 0
       | otherwise   = -1

vabs1 :: (Ord a, Num a) => a -> a
vabs1 a | a >= 0   = a
        | a < 0    = -a

vabs2 :: (Ord a, Num a) => a -> a
vabs2 a | sign a == -1   = -a
        | otherwise      = a

pot :: (Eq a, Integral a, Num b) => a -> b -> b
pot a b = b ^ a
-- pot a b | a == 1      = b
--         | otherwise   = pot (a - 1) (b * b) 

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _     _    = False

max3 :: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c

swap :: (Num a, Num b) => (a,b) -> (b,a)
swap (a,b) = (b,a)

-- 3] --------------------------------------------------------------------------------------
esBisiesto :: Int -> Bool
esBisiesto n = (mod n 4 == 0) && (mod n 100 /= 0 || mod n 400 == 0)

-- 4] --------------------------------------------------------------------------------------
(*$) :: Num a => [a] -> a -> [a]
ns *$ n = map (* n) ns

-- 5] --------------------------------------------------------------------------------------
divisors :: Integral a => a -> [a]
divisors n = [x | x <- [1..n], (mod n x) == 0] 

matches :: Eq a => a -> [a] -> [a]
matches n ms = [x | x <- ms, x == n]

cuadrupla :: (Num a, Enum a, Eq a) => a -> [(a,a,a,a)]
cuadrupla n = [(x,y,z,w) | x <- [0..n], y <- [0..n], z <- [0..n], w <- [0..n],
                       (x^2) + (y^2) == (z^2) + (w^2)]

unique :: Eq a => [a] -> [a]
unique xs = [x | (x,i) <- zip xs [0..], not(elem x (take i xs))]

-- 6] --------------------------------------------------------------------------------------
scalarProduct :: (Num a) => [a] -> [a] -> a
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys] 

-- 7] --------------------------------------------------------------------------------------
suma :: Num a => [a] -> a
suma []       = 0
suma (x:xs)   = x + suma xs

alguno :: [Bool] -> Bool
alguno []       = False
alguno (x:xs)   = x || alguno xs

todos :: [Bool] -> Bool
todos []       = False
todos [x]      = x
todos (x:xs)   = x && todos xs

codes :: [Char] -> [Int]
codes []       = []
codes (x:xs)   = ord x : codes xs

restos :: Integral a => [a] -> a -> [a]
restos [] n       = []
restos (x:xs) n   = (mod x n) : (restos xs n)

cuadrados :: Num a => [a] -> [a]
cuadrados []       = []
cuadrados (x:xs)   = (x^2) : cuadrados xs

longitudes :: Num b => [[a]] -> [b]
longitudes []         = []
longitudes (xs:xss)   = length2 xs : longitudes xss

orden :: (Num a, Ord a) => [(a,a)] -> [(a,a)]
orden []     = []
orden (x:xs) | fst x < (snd x * 3)   = x : orden xs
             | otherwise             = orden xs

pares :: Integral a => [a] -> [a]
pares []     = []
pares (x:xs) | mod x 2 == 0   = x : pares xs
             | otherwise      = pares xs

letras :: [Char] -> [Char]
letras []     = []
letras (x:xs) | isLetter x   = x : letras xs
              | otherwise    = letras xs

masDe :: (Ord b, Num b) => [[a]] -> b -> [[a]]
masDe [] _       = []
masDe (xs:xss) n | (length2 xs) > n   = xs : masDe xss n
                 | otherwise          = masDe xss n

