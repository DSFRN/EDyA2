module Lab1C where
 
import Data.List
import Data.Ord

type Texto = String

{-
   Definir una función que dado un caracter y un texto calcule la frecuencia 
   con la que ocurre el caracter en el texto
   Por ejemplo frecuency 'a' casa = 0.5 
-}

countEq :: Char -> Texto -> Float
countEq c []     =  0.0
countEq c (x:xs) =  if (c == x)
                    then (1.0 + countEq c xs)
                    else countEq c xs

frecuency :: Char -> Texto -> Float
frecuency c xs = (countEq c xs) / fromIntegral (length xs)

{-
  Definir una función frecuencyMap que dado un texto calcule la frecuencia 
  con la que ocurre cada caracter del texto en éste.
  La lista resultado debe estar ordenada respecto a la frecuencia con la que ocurre 
  cada caracter, de menor a mayor frecuencia. 
    
  Por ejemplo frecuencyMap casa = [('c',0.25),('s',0.25),('a',0.5)]

-}

eliminateDup :: Eq a => [a] -> [a]
eliminateDup []     =  []
eliminateDup (x:xs) =  x : eliminateDup (filter (/= x) xs)

freqChar :: Texto -> Texto -> [(Char, Float)]
freqChar [] _      =  []
freqChar (x:xs) ys =  (x, frecuency x ys) : (freqChar xs ys)

freqAux :: [(Char, Float)] -> [(Char, Float)]
freqAux []         =  []
freqAux [x]        =  x : []
freqAux (x:(y:zs)) =  if compare (snd x) (snd y) == LT
                      then x : freqAux (y:zs)
                      else y : freqAux (x:zs)

frecuencyMap :: Texto -> [(Char, Float)]
frecuencyMap xs = freqAux (freqChar (eliminateDup xs) xs)

{-
  Definir una función subconjuntos, que dada una lista xs devuelva una lista 
  con las listas que pueden generarse con los elementos de xs.

  Por ejemplo subconjuntos [2,3,4] = [[2,3,4],[2,3],[2,4],[2],[3,4],[3],[4],[]]
-}

subconjuntos :: [a] -> [[a]]
subconjuntos []     =  [[]]
subconjuntos (x:xs) =  map (x:) (subconjuntos xs) ++ subconjuntos xs

{-
 Definir una función intercala  a -> [a] -> [[a]]
 tal que (intercala x ys) contiene las listas que se obtienen
 intercalando x entre los elementos de ys. 
 
 Por ejemplo intercala 1 [2,3]  =  [[1,2,3],[2,1,3],[2,3,1]]
-}

intercalAux :: a -> [a] -> Int -> [[a]]
intercalAux x ys i = if i <= length ys
                     then (take i ys ++ [x] ++ drop i ys) : intercalAux x ys (i+1)
                     else []

intercala :: a -> [a] -> [[a]]
intercala x ys =  intercalAux x ys 0

{-
  Definir una función permutaciones que dada una lista calcule todas las permutaciones
  posibles de sus elementos. Ayuda la función anterior puede ser útil. 

  Por ejemplo permutaciones abc = [abc,bac,cba,bca,cab,acb]
-}

permutAux :: [a] -> Int -> [[[a]]]
permutAux [] _ = []
permutAux (x:xs) i = if i < length (x:xs)
                     then intercala x xs : permutAux (xs ++ [x]) (i+1)
                     else []

permutaciones :: Eq a => [a] -> [[a]]
permutaciones xs = eliminateDup (concat (permutAux xs 0))

