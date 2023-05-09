import Data.IntMap (size)
import Text.XHtml (height)
import Distribution.Simple.Utils (xargs)



{-



Definir las siguientes funciones sobre  ́arboles binarios: size y height

-}

--1. Definir el tipo Nat, visto en el teorico.
data Nat = Zero | Succ Nat  


--2. Definir la funci ́on natT oInt : N at → Int que dado un n ́umero Nat retorna
--su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2.


natToInt :: Nat -> Int 
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n


instance Show Nat where 
    show = show.natToInt


instance Eq Nat where 
    n == m = (natToInt n) == (natToInt m )
 
--3. Definir la funci ́on intT oN at : Int → N at que dado un n ́umero entero retorna

intToNat :: Int -> Nat 
intToNat 0 = Zero 
intToNat n = Succ (intToNat (n-1))

--4. Definir la funci ́on sumaN at : N at → N at → N at, la cual suma dos n ́umeros Nat.


sumaNat :: Nat -> Nat -> Nat 
sumaNat Zero Zero = Zero
sumaNat (Succ n) Zero = n
sumaNat Zero (Succ n) = n
sumaNat (Succ n) (Succ m) = intToNat(natToInt((Succ n)) + natToInt((Succ m)))




--5. Definir los  ́arboles binarios.


data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)


instance (Show a) => Show (Arbol a) where 
    show Vacio = "Vacio"
    show (Nodo r hi hd) = "< " ++ show hi ++ " , " ++ show r ++ " , " ++ show hd ++ " > "

--6. La funci ́on size, que dado un  ́arbol retorna el n ́umero de nodos del  ́arbol.


sizeArbol :: Arbol a -> Int
sizeArbol Vacio = 0
sizeArbol (Nodo r hi hd) = 1 + sizeArbol hi + sizeArbol hd


--7. La funci ́on height, que dado un  ́arbol retorna la altura del mismo.

max2 :: (Ord a) => a -> a -> a
max2 x y
       | x > y = x
       | otherwise = y


alt :: Arbol a -> Int
alt Vacio = 0
alt (Nodo r hi hd) = 1 + max2 (alt hi) (alt hd)



preorden :: Arbol a -> [a]
preorden Vacio = []
preorden (Nodo r hi hd) = [r] ++ preorden hi ++ preorden hd



inorden :: Arbol a -> [a]
inorden Vacio = []
inorden (Nodo r hi hd) = inorden hi ++ [r] ++ inorden hd


postorden :: Arbol a -> [a]
postorden Vacio = []
postorden (Nodo r hi hd) = postorden hi ++ postorden hd ++ [r]