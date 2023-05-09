

{-



-}




--Ejercicio 1.[2.5pt] Dado un  ́arbol binario de enteros (de la forma que fueron definidos en

--clases), definir una funci ́on en Haskell f:BinTree Int -> Int que retorne la suma de la infor-
--maci ́on en los nodos.



data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a)



sumaRaices :: (Num a) =>Arbol a -> a 
sumaRaices Vacio = 0
sumaRaices (Nodo hi r hd) = r + sumaRaices hi + sumaRaices hd



--Escriba un  ́arbol que contenga los 4 primeros digitos de su DNI.

--(Nodo((Nodo Vacio 3 Vacio) 4 (Nodo Vacio 3 (Nodo Vacio 6 Vacio))))



--Ejercicio 2.[2.5pt] Dado el siguiente tipo de datos definidos en Haskell.


data SIntExp = Cte Int | Sum SIntExp SIntExp | Mul SIntExp SIntExp deriving Show
--Dar 4 expresiones que sean del tipo dado,

-- Cte :: Int -> SIntExp
-- Sum :: SIntExp -> SIntExp -> SIntExp
-- Mul :: SIntExp -> SIntExp -> SIntExp

-- (Sum (Cte 2) (Cte 2))
-- Cte 3
-- (Mul (Cte 1) (Cte 8))
-- (Mul (Sum (Cte 2) (Cte 1)) (Cte 4))



--Con el tipo dado dar una expresi ́on correspondiente a d0 + (d1 ∗ d2). En donde d0, d1, d2
--son los tres primeros digitos de su DNI.


-- (Sum (Cte 4) Mul ((Cte 3) (Cte 3)))

--Ejercicio 3.[2.5pt] Defina la funci ́on compact:: [Int] -> [Int] que elimina repeticiones
--consecutivas de un elemento. Por ejemplo: compact [2,2,3,1,3] = [2,3,1,3]. Aplique la
--funci ́on a los digitos de su DNI, y muestre las reducciones que har ́ıa Haskell.


compact :: (Eq a) => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:ys)
               | x == y = compact (y:ys)
               | otherwise = x : compact (y:ys)


{-

compact [4,3,3,6,7,5,7,3]
        4 : compact [3,3,6,7,5,7,3]
                    compact [3,6,7,5,7,3]
                            3 : compact [6,7,5,7,3]
                                    6 : compact [7,5,7,3]
                                             7 : compact [5,7,3]
                                                    5 : compact [7,3]
                                                            7 : compact [3]
                                                                    [3]


compact [4,3,3,6,7,5,7,3]
={compact, 4 /= 3}
    4 : compact [3,3,6,7,5,7,3]
={compact, 3 == 3}
    4 : compact [3,6,7,5,7,3]
={compact, 3 /= 6}
    4 : 3 : compact [6,7,5,7,3]
{compact, 6 /= 7}
    4 : 3 : 6 : compact [7,5,7,3]
{compact, 7 /= 5}
    4 : 3 : 6 : 7 : compact [5,7,3]
{compact, 5 /= 7}
    4 : 3 : 6 : 7 : 5 : compact [7,3]
{compact, 7 /= 3} 
    4 : 3 : 6 : 7 :5 : 7 : compact [3]
{compact, Caso Base}
    4 : 3 : 6 : 7 : 5 : 7 : [3]









-}



{-
Ejercicio 4.[2.5pt] Dadas las siguientes definiciones:
square :: Int -> Int
square x = x*x
and :: Bool -> Bool -> Bool
and true y = y
and false x = false
inf :: Int -> Int
inf = inf+1
Elija solo dos de las siguientes expresiones y aplique las eveluaciones normal y aplicativa.
Cu ́ales de ellas tienen formal normal y cu ́ales no? Justificar su respuesta.

(square inf) + (square inf)
and (inf == inf) ((square 2) == 4)
and ((square 2) == 5) (inf == inf)


Orden Aplicativo: se reduce siempre la expresión más
adentro (de izquierda a derecha)

Orden Normal: se reduce siempre la expresión más afuera
y más a la izquierda


Forma Normal mas afuera y mas a la izquierda
(sq inf) + (sq inf)
=def de sq 
(inf * inf) + (sq inf) 
=def de inf
(inf+1 * inf) + (sq inf)
=def de inf
(inf+1+1 * inf) + (sq inf)


Forma aplicativa mas adentro y mas a la izquierda 
(sq inf) + (sq inf) 
=def de inf
(sq inf) + (sq inf+1)

(sq inf) + (sq inf+1+1)

(sq inf) + (sq inf+1+1+1)

(sq inf) + (sq inf+1+1+1+1)

-}





square :: Int -> Int
square x = x * x

inf :: Int
inf = (1+inf)

and' :: Bool -> Bool -> Bool
and' True y = y 
and' False x = False 