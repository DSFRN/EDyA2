module Prac3 where

-- (1)
type Color = (Int, Int, Int)

mezclar :: Color -> Color -> Color
mezclar (r1, g1, b1) (r2, g2, b2) = (prom r1 r2, prom g1 g2, prom b1 b2)
                                    where prom x y = div (x + y) 2

-- (2)
type Linea = (String, Int)

vacia :: Linea
vacia = ([],0)

moverIzq :: Linea -> Linea
moverIzq (s,0) = (s,0)
moverIzq (s,n) = (s,n-1)

moverDer :: Linea -> Linea
moverDer (s,n) = if n < length s
                 then (s,n+1)
                 else (s,n)

moverIni :: Linea -> Linea
moverIni (s,_) = (s,0)

moverFin :: Linea -> Linea
moverFin (s,_) = (s,length s)

insertar :: Char -> Linea -> Linea
insertar c (s,n) = (take n s ++ [c] ++ drop n s, n+1)

borrar :: Linea -> Linea
borrar (s,0) = (s,0)
borrar (s,n) = (take (n-1) s ++ drop n s, n-1)

-- (3)
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
-- headCL y tailCL no estan definidos para una lista vacia.
-- headCL toma una CList y devuelve el primer elemento de la misma (el de mas a la izquierda).
-- tailCL toma una CList y devuelve la misma sin el primer elemento.

isEmptyCL :: CList a -> Bool
isEmptyCL (EmptyCL) = True
isEmptyCL _         = False

isCUnit :: CList a -> Bool
isCUnit (CUnit _) = True
isCUnit _         = False

headCL :: CList a -> a
headCL (CUnit a) = a
--headCL (Consnoc a (CList a) a) = Preguntar

tailCL :: CList a -> CList a
tailCL (CUnit a) = EmptyCL
--tailCL (Consnoc a (CList a) a) = Preguntar

-- (4)
data Exp = Lit Int | Add Exp Exp | Sub Exp Exp | Prod Exp Exp | Div Exp Exp

eval :: Exp -> Int
eval (Add a b)  = eval a + eval b
eval (Sub a b)  = eval a - eval b
eval (Prod a b) = eval a * eval b
eval (Div a b)  = div (eval a) (eval b)
eval (Lit a)    = a

-- (5)
-- Paja

-- (6)
-- a) Considere el evaluador eval :: Exp → Int del ejercicio (4) ¿Como maneja los errores de division por 0?
--    -> Nos retorna una excepción: "Exception: divide by zero".
-- b) Defina un evaluador seval :: Exp → Maybe Int para controlar los errores de division por 0.
seval :: Exp -> Maybe Int
seval (Div _ (Lit 0)) = Nothing
seval exp             = Just (eval exp)
