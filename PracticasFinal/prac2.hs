{-NOTA Los ejercicios con * son para resolver en su casa
1. Leer los captulos 1 y 2 del libro Aprende Haskell por el bien de todos!




6 *. Defina una función edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int
que dada dos fechas indica los años transcurridos entre ellas. Por ejemplo:
edad (20,10,1968) (30,4,1987) = 18






-}





{-2. Definir las siguientes funciones:
• hd ::[A] -> A retorna el primer elemento de una lista.
• tl ::[A] -> [A] retorna toda la lista menos el primer elemento.
• last ::
[A] ->
A retorna el último elemento de la lista.
• init:: [A] -> [A] retorna toda la lista menos el último elemento.-}


hd :: [a] -> a 
hd [] = error "lista vacia"
hd (x:xs) = x


tl :: [a] -> [a]
tl [] = []
tl (x:xs) = xs


lastt :: [a] -> a 
lastt [] = error "lista vacia"
lastt [x] = x
lastt (x:xs) = lastt xs


initt :: [a] -> [a]
initt [] = error "lista vacia"
initt [x] = []
initt (x:xs) = x : initt xs




{-3. Defina una función máximo de tres, tal que maxTres x y z es el máximo
valor entre x, y, z . Por ejemplo: maxTres 6 7 4 = 7.-}

max2 :: (Ord a,Num a) => a -> a -> a 
max2 x y | x > y = x
         | otherwise = y 



max3 :: (Num a,Ord a) => a -> a -> a -> a
max3 x y z = (max2 x (max2 y z))




{-4. Defina las siguientes operaciones sobre listas (vistas en el teórico): concate-
nar, tomar, tirar y C.-}


concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar xs [] = xs
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys 



tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 xs = []
tomar n (x:xs) = x :tomar (n-1) xs



tirar :: Int -> [a] -> [a]
tirar _ [] = []
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs 



concatenarFinal :: a -> [a] -> [a]
concatenarFinal x [] = [x]
concatenarFinal x [y] = [y,x]
concatenarFinal x (y:ys) = y : concatenarFinal x ys 



{-5. Defina una función abs: Int -> Int que calcula el valor absoluto de un
número.-}


valorabs :: Int -> Int 
valorabs x | x >= 0 = x
           | otherwise = x*(-1)




{-7. La disyunción excluyente xor de dos fórmulas se verifica si una es verdadera
y la otra es falsa. Defina la función xor que calcule la disyunción excluyente a
partir de la tabla de verdad.
*. Ahora defina la función xor2 que calcule la disyunción excluyente pero sin
que considere todos los posibles valores de las entradas. Cuál será la diferencias
entre ambas definiciones?-}



xor :: Bool -> Bool -> Bool
xor True True = False
xor False True = True 
xor True False = True
xor False False = False 


xor2 :: Bool -> Bool -> Bool
xor2 False False = False
xor2 True True = False  
xor2 _ _ = True 



{-8. Defina una función que dado
 un número natural, decida si el mismo es primo
o no.-}


divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], mod n x == 0]


primo :: Int -> Bool
primo x = (length (divisores x)) == 2



{-9 *. Defina una función que dado un número natural n, retorne la lista de todos
los números naturales primos menores que n.-}





primosMenores :: Int -> [Int]
primosMenores n = [x | x <- [1..n], primo x, x < n]



{-10. Defina una función que dada una lista, retorne la reversa de la misma.-}


reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]




{-
11. Defina una función que dadas dos listas, decida si las listas son iguales.-}



iguales :: (Eq a) => [a] -> [a] -> Bool
iguales [] [] = True
iguales xs [] = False
iguales [] ys = False 
iguales (x:xs) (y:ys) = (x == y) && iguales xs ys 



{-12 *. Defina una función que dada una lista decida si es un palı́ndromo o no.-}


palindromo :: (Eq a)=> [a] -> Bool
palindromo [] = True 
palindromo xs = iguales xs (reversa xs)


{-
13. Defina una función que dados tres números a, b, c devuelva la cantidad de
raı́ces reales de la ecuación ax2 + bx + c = 0-}


cantRaices :: (Num a,Ord a) => a -> a -> a -> Int 
cantRaices a b c | disc == 0 = 1
           | disc > 0 = 2
           | otherwise = 0
           where disc = (b*b)-4*(a)*(c)