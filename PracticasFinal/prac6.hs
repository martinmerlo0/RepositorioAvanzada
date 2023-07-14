{-Práctico 6: Programación Funcional: Tipos de datos recursivos




Definir las siguientes funciones sobre árboles binarios: size y height

7. La función height, que dado un árbol retorna la altura del mismo.-}



data Nat = Zero | Succ Nat deriving Show



{-
2. Definir la función natT oInt : N at → Int que dado un número Nat retorna
su entero correspondiente. Por ejemplo: natToInt (Suc(Suc Zero)) = 2.-}


natToInt :: Nat -> Int 
natToInt Zero = 0 
natToInt (Succ n) = 1 + natToInt n



{-3. Definir la función intT oN at : Int → N at que dado un número entero retorna
su Nat correspondiente. Por ejemplo: intToNat 2 = (Suc(Suc Zero)).Int -}



intToNat :: Int -> Nat 
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))


{-4. Definir la función sumaN at : N at → N at → N at, la cual suma dos números Nat.-}




sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat (Succ n) Zero = n
sumaNat Zero (Succ m) = m
sumaNat (Succ m) (Succ n) =(intToNat (natToInt (Succ m) + natToInt (Succ n)))




{-5. Definir los árboles binarios.-}


data Arbol a = Vacio | Nodo (Arbol a) a (Arbol a) deriving Show 



{-6. La función size, que dado un árbol retorna el número de nodos del árbol.-}

size :: Arbol a -> Int 
size Vacio = 0 
size (Nodo hi r hd) = 1 + size hi + size hd 