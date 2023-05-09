{-






-}

--1. Definir el tipo Nat, visto en el teórico.

data Nat = Zero | Succ Nat deriving Show


--2. Definir la función natToInt : Nat → Int que dado un número Nat retorna
--su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2.
natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n 




--3. Definir la función intT oN at : Int → N at que dado un número entero retorna
--su Nat correspondiente. Por ejemplo: intToNat 2 = (Suc(Suc Zero)).


intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ(intToNat(n-1))




--4. Definir la función sumaN at : N at → N at → N at, la cual suma dos números Nat.


sumaNat :: Nat -> Nat -> Nat
sumaNat Zero n = n
sumaNat (Succ m) n = Succ (sumaNat m n)



--5. Definir los árboles binarios.
--Definir las siguientes funciones sobre árboles binarios: size y height



data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a)
--                 		Hijo izq     raiz     hijo der


instance (Show a) => Show (Arbol a) where
 show Vacio = "Ø"
 show (Nodo hi r hd) = "<" ++ show hi ++" , " ++ show r ++ " , " ++ show hd ++">" 



--6. La función size, que dado un árbol retorna el número de nodos del árbol.


size :: Arbol a -> Int 
size Vacio = 0 
size (Nodo hi r hd) = 1 + size(hi) + size(hd)



--7. La función height, que dado un árbol retorna la altura del mismo.


height :: Arbol a -> Int
height Vacio = 0
height (Nodo hi r hd) = 1 + 
 
{-
instance (Show a) => Show (BinTree a) where
  show Nil = "Vacio"
  show (Nodo hi raiz hd) = "< Hijo izquierdo : " ++ show hi ++ ",  Raiz : " ++ show raiz ++ ",  Hijo derecho : " ++ show hd ++ ">"
-}