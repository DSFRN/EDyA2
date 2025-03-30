-- (1) Dar el tipo completo de las siguientes funciones:
-- a) test, donde
      test :: (Num a, Eq a) => (a -> a) -> a -> Bool
      test f x = (f x) == (x + 2)
-- b) esMenor, donde
      esMenor :: Ord a => a -> a -> Bool
      esMenor y z = y < z
-- c) eq, donde
      eq :: Eq a => a -> a -> Bool
      eq a b = a == b
-- d) showVal, donde
      showVal :: Show a => a -> String
      showVal x = "Valor:" ++ show x 




-- (2) Dar el tipo de las siguientes operaciones y explicar su propósito:
-- a) Suma 5 a un número.
      (+5) :: Num a => a -> a
      (+5) x = x + 5

-- b) Determina si un número es positivo.
      (0<) :: Ord a => a -> Bool
      (0<) x = x > 0

-- c) Concatena una 'a' al inicio de una cadena.
      ('a':) :: String -> String
      ('a':) xs = 'a' : xs

-- d) Agrega un salto de linea al final de una cadena.
      (++"\n") :: String -> String
      (++"\n") xs = xs ++ "\n"

-- e) Retorna una lista con sólo 7's.
      filter (==7) :: Eq a => a -> Bool
      filter (==7) xs = [x | x <- xs, x == 7]

-- f) Agrega el número 1 a cada sublista de una lista.
      map (++[1]) :: Num a => [[a]] -> [[a]]
      map (++[1]) xss = [xs ++ [1] | xs <- xss]




-- (3) Dar al menos dos ejemplos de funciones que tengan el tipo indicado en cada caso:
-- a) (Int -> Int) -> Int
-- b) Int -> (Int -> Int)
-- c) (Int -> Int) -> (Int -> Int)
-- d) Int -> Bool
-- e) Bool -> (Bool -> Bool)
-- f) (Int, Char) -> Bool
-- g) (Int, Int) -> Int
-- h) Int -> (Int -> Int)
-- i) a -> Bool
-- j) a -> a




-- (4) Indicar si las expresiones están bien formadas o no.
--     En caso afirmativo, dar lo que retorna.
--     En caso negativo, especificar si es un Error sintáctico o de tipos.

      if true then false else true where false = True;true = False -- (a)
--    -> False.

      if if then then else else -- (b)
--    Error sintáctico.

      False == (5 >= 4) -- (c)
--    -> False.

      1 < 2 < 3 -- (d)
--    Error de tipos.

      1 + if ('a' < 'z') then -1 else 0 -- (e)
--    -> 0.

      if fst p then fst p else snd p where p = (True, 2) -- (f)
--    Error de tipos.

      if fst p then fst p else snd p where p = (True, False) -- (g)
--    -> True




-- (5) Reescribir cada una de las siguientes definiciones sin usar let, where o if:
-- a) f x = let (y, z) = (x, x) in y
      f x = x

-- b) greater (x, y) = if x > y then True else False
      greater (x, y) = x > y

-- c) f (x, y) = let z = x + y in g (z, b) where g (a, b) = a - b
      f (x, y) = (x + y) - y




-- (6) Pasar de notación Haskell a notación Lambda.
-- a) smallest, tal que
--    smallest (x,y,z) | x <= y && x <= z = x
--                     | y <= x && y <= z = y
--                     | z <= x && z <= y = z

      smallest = \x y z -> min (min x y) z

-- b) second x = \x -> x
      second = \_ -> (\x -> x)

-- c) andThen, tal que
--    andThen True y = y
--    andThen False y = False

      andThen = \b y -> if b then y else b

-- d) twice f x = f (f x)
      twice = \f x -> f (f x)

-- e) flip f x y = f y x
      flip = \f x y -> f y x

-- f) inc = (+1)
      inc = \x -> x + 1




-- (7) Pasar de notación Lambda a notación Haskell.
-- a) iff = \x -> \y -> if x then not y else y
      iff x y = if x then not y else y

-- b) alpha = \x -> x
      alpha x = x




-- (8) Suponiendo que f y g tienen los tipos
        f :: c -> d
        g :: a -> b -> c

--     y sea h definida como
        h x y = f (g x y)

--     Determinar el tipo de h e indicar cuáles de las sigientes definiciones
--     son equivalentes a la dada:
          h   =    f . g     -- (i)
         h x  =  f . (g x)   -- (ii)
        h x y = (f . g) x y  -- (iii)
--
--     Dar el tipo de la función (.)

       h :: x -> y -> d  --   h <-> (iii)
       (.) :: (b -> c) -> (a -> b) -> a -> c



       
-- (9) La función zip3 zipea 3 listas.
--     Programar una versión recursiva y otra que utilice la función zip.
       zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]

       zip3 [] ys zs = []
       zip3 xs [] zs = []
       zip3 xs ys [] = []
       zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs

       zip3 (x:xs) (y:ys) (z:zs) = emparejar $ zip $ zip xs ys zs where
                                   emparejar [] = []
                                   emparejar ((x,y),z) : ls) = (x,y,z) : emparejar ls




-- (10) Indicar bajo qué supocisiones tienen sentido las siguientes ecuaciones.
--      Para aquellas que tengan sentido, indicar si son verdaderas.
--      En caso contrario modificar su lado derecho para volverlas verdaderas.

-- a)  [[]] ++  xs   =  xs
       xs = []       => [[]]
       xs = [[a]]    => [[], [a]]
 
-- b)  [[]] ++  xs   =  [xs]
       xs = []       => [[]]      -- (=) vale sólo para xs == [].
       xs = [[a]]    => [[], [a]]

-- c)  [[]] ++  xs   =  [] : xs
       xs = []       => [[]]
       xs = [[a]]    => [[], xs]  -- (=) vale sólo para xs == [[]].

-- d)  [[]] ++  xs   =  [[], xs]
       xs = []       => [[]]
       xs = [[a]]    => [[], xs]  -- (=) vale en este caso.

-- e)  [[]] ++ [xs]  =  [[], xs]
       xs = [a]      => [[], xs]  -- (=) vale en este caso.

-- f)  [[]] ++ [xs]  =  [xs]
       xs = [a]      => [[], xs]
       la igualdad no se cumple para ningún xs.
--     versión correcta:
--     [[]] ++ [xs]  =  [[], xs]

-- g)  []  ++  xs    =  [] : xs
       xs = [a]      => [a] = a : []
       xs = [[a]]    => [[a]]
       la igualdad no se cumple para ningún xs.
--     versión correcta:
--     []  ++  xs    =  xs

-- h)  []  ++  xs    =  xs
       la igualdad se cumple para cualquier xs.    

-- i)  [xs] ++  []   =  [xs]
       la igualdad se cumple para cualquier xs.

-- j)  [xs] ++ [xs]  =  [xs, xs]
       la igualdad se cumple para cualquier xs.




-- (11) Inferir, de ser posible, los tipos de las siguientes funciones:
--      (puede suponer que sqrt :: Float -> Float)

--    modulus :: Floating a => [a] -> a
      modulus vs = sqrt . sum . map (^2) vs   -- (a)

--    vmod :: Floating a => [[a]] -> [a]
      vmod []       = []                      -- (b)
      vmod (vs:vss) = modulus vs : vmod vss




-- (12) Dado el siguiente tipo para representar números binarios:
         type NumBin = [Bool]

--      donde False <=> 0 y True <=> 1.
--      Definir la siguientes operaciones tomando como convecnión una representación
--      Little-Endian.
--
--      a) Suma binaria.
--      b) Producto binario.
--      c) Cociente y resto de la división por dos.

-- (13) & (14) Resueltos en lab01.hs
