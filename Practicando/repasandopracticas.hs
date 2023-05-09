
--PRACTICA 2







{-2. Definir las siguientes funciones:
• hd :: [A] -> A retorna el primer elemento de una lista.
• tl :: [A] -> [A] retorna toda la lista menos el primer elemento.
• last :: [A] -> A retorna el  ́ultimo elemento de la lista.
• init:: [A] -> [A] retorna toda la lista menos el  ́ultimo elemento.-}


hd :: [a] -> a
hd [] = error "no puedo devolver nada de una lista vacia"
hd (x:_) = x


tl :: [a] -> [a]
tl [] = error "Lista vacia"
tl [x] = []
tl (_:xs) = xs


last' :: [a] -> a
last' [] = error "lista vacia"
last' [x] = x
last' (_:xs) = last xs


init' :: [a] -> [a]
init' [] = error "lista vacia"
init' [x] = []
init' (x:xs) = x : init' xs

{-3. Defina una funci ́on m ́aximo de tres, tal que maxTres x y z es el m ́aximo
valor entre x, y, z . Por ejemplo: maxTres 6 7 4 = 7.-}



max2 :: (Ord a) => a -> a -> a
max2 x y
       | x > y = x
       | otherwise = y 


max3 :: (Ord a) => a -> a -> a -> a
max3 x y z = max2 x (max2 z y) 


{-4. Defina las siguientes operaciones sobre listas (vistas en el te ́orico): concate-
nar, tomar, tirar y ConcFinal.-}



concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar [] ys = ys
concatenar (x:xs) ys = x : concatenar xs ys 



tomar :: Int -> [a] -> [a]
tomar 0 xs = []
tomar n (x:xs) = x : tomar (n-1) xs


tirar :: Int -> [a] -> [a]
tirar 0 [] = []
tirar 0 xs = xs
tirar n (x:xs) = tirar (n-1) xs



concFinal :: a -> [a] -> [a] 
concFinal e [] = [e]
concFinal e [x] = [x,e]
concFinal e (x:xs) = x : concFinal e xs

{-5. Defina una funci ́on abs: Int -> Int que calcula el valor absoluto de un
n ́umero.-}



valorabs :: Int -> Int
valorabs x
        | x > 0 = x
        | otherwise = -1 * x


{-6 *. Defina una funci ́on edad :: (Nat,Nat,Nat) -> (Nat,Nat,Nat) -> Int
que dada dos fechas indica los a ̃nos transcurridos entre ellas. Por ejemplo:

edad (20,10,1968) (30,4,1987) = 18
-}



edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int
edad (d,m,a) (d1,m1,a1)
                    | d == d1 && m == m1 = cuenta
                    | d < d1 && m == m1 = cuenta
                    | d > d1 && m == m1 = cuenta -1
                    | m > m1 = cuenta -1
                    | m < m1 = cuenta 
                    where cuenta = valorabs(a-a1)




{-7. La disyunci ́on excluyente xor de dos f ́ormulas se verifica si una es verdadera
y la otra es falsa. Defina la funci ́on xor que calcule la disyunci ́on excluyente a
partir de la tabla de verdad.
*. Ahora defina la funci ́on xor2 que calcule la disyunci ́on excluyente pero sin
que considere todos los posibles valores de las entradas. Cu ́al ser ́a la diferencias
entre ambas definiciones?-}


xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor True False = True
xor False True = True 



xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 False False = False
xor1 _ _ = True


{-8. Defina una funci ́on que dado un n ́umero natural, decida si el mismo es primo
o no.-}



divisores :: Int -> [Int]
divisores n = [x | x <-[1..n], mod n x == 0]


esPrimo :: Int -> Bool
esPrimo n = length(divisores n) == 2



{-9 *. Defina una funci ́on que dado un n ́umero natural n, retorne la lista de todos
los n ́umeros naturales primos menores que n.-}



primoshasta :: Int -> [Int]
primoshasta n = [x | x <-[1..n], esPrimo x]




{-10. Defina una funci ́on que dada una lista, retorne la reversa de la misma.-}


reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = last' (x:xs) : reversa (init'(x:xs))


{-11. Defina una funci ́on que dadas dos listas, decida si las listas son iguales.-}


esta :: (Eq a ) => a -> [a] -> Bool
esta e [] = False
esta e (x:xs)
            | e == x = True
            | otherwise = esta e xs 



iguales :: (Eq a ) =>[a] -> [a] -> Bool
iguales [] [] = True
iguales [] ys = False
iguales (x:xs) (y:ys) = x == y && iguales xs ys 



{-12 *. Defina una funci ́on que dada una lista decida si es un pal ́ındromo o no.-}



palindromo :: (Eq a) => [a] -> Bool
palindromo [] =  True 
palindromo xs = xs == reversa xs



{-

13. Defina una funci ́on que dados tres n ́umeros a, b, c devuelva la cantidad de
ra ́ıces reales de la ecuaci ́on ax2 + bx + c = 0--}


raices :: Float -> Float -> Float -> Int
raices a b c 
           | disc > 0 = 2
           | disc == 0 = 1 
           | otherwise = 0
           where disc = (b * b) -4 * a * c













--PRACTICA 3











{-1. Define una funci ́on que, dadas dos listas ys y xs de naturales ordenadas,
retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
elementos de ys y xs.-}

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge xs ys = quicksort (xs ++ ys)


{-2. Define una funci ́on que, dada una lista de naturales, la ordene.-}


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
             where menores = [y | y <-xs , y <= x]
                   mayores = [y | y <-xs , y > x]


{-3. Define una funci ́on que, recursivamente y s ́olo utilizando adici ́on y multipli-
caci ́on, calcule, dado un natural n, el n ́umero 2n.
-}



potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 1 = 2
potencia2 n = 2 * potencia2 (n-1)




{-4. Define una funci ́on que, dado un n ́umero natural n, retorne su representaci ́on
binaria como secuencia de bits.-}




binaria :: Int -> [Int]
binaria 0 = []
binaria n = mod n 2 : binaria (div n 2)


binaria' :: Int -> [Int]
binaria' n = reversa (binaria n)





{-5 *. Define una funci ́on que, dado un n ́umero natural n en su representaci ́on
binaria, decida si n es par o no.-}


binpar :: [Int] -> Bool
binpar [] = error "lista vacia"
binpar xs
        | last' xs == 1 = False 
        | otherwise = True





{-6. Define la funci ́on que retorne la distancia de Hamming: dadas dos listas es el
n ́umero de posiciones en que los correspondientes elementos son distintos. Por
ejemplo: distanciaH ”roma””camino”− > 3
distanciaH ”romano””rama”− > 1-}



distanciaH :: String -> String -> Int
distanciaH [] [] = 0
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys)
                      | x == y = distanciaH xs ys
                      | otherwise = 1 + distanciaH xs ys 






{-7. Define la funci ́on que, dado un n ́umero natural, decida si el mismo es un
cuadrado perfecto o no.-}





{-
8. Define la funci ́on repetidos de forma tal que dado un elemento z y un entero
n; z aparece n veces.-}


repetidos :: a -> Int -> [a]
repetidos z 0 = []
repetidos z n = z : repetidos z (n-1)




{-9. Define la funci ́on nelem tal que nelem xs n es elemento en ́esimo de xs,
empezando a numerar desde el 0. Por ejemplo:
nelem [1, 3, 2, 4, 9, 7] 3 -> 4
-}



nelem :: [a] -> Int -> a
nelem [] n = error "lista vacia o posicion invalida"
nelem xs 0 = hd xs
nelem (x:xs) n = nelem xs (n-1)



{-10 *. Define la funci ́on posicionesC tal que posicionesC xs c es la lista de la
posiciones del caracter c en la cadena xs. Por ejemplo:
posicionesC ”Catamarca”
0a
0− > [1, 3, 5, 8]-}


posicionesC :: String -> Char -> Int -> [Int]
posicionesC [] c n= []
posicionesC (x:xs) c n
                | x == c = n : posicionesC xs c (n+1)
                | otherwise = posicionesC xs c (n+1)


{-

11. Define la funci ́on compact, dada una lista retorna la lista sin los elementos
repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3]-}


compact :: (Eq a) => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:ys)
               | x == y = compact (y:ys)
               | otherwise = x : compact (y:ys)







--PRACTICA 5






{-









12 *. Redefinir la funci ́on and tal que and xs se verifica si todos los ele-
mentos de xs son verdaderos. Por ejemplo: and [1<2, 2<3, 1/=0] = True,
and [1<2, 2<3, 1 == 0] = False.





Utilizando listas por comprensi ́on resolver:







24. Definir la lista infinita de los n ́umeros pares.-}





--1. Generar una lista infinita de unos.


inf1 :: [Int]
inf1 = 1:inf1



--2. Generar una lista infinita de naturales comenzando desde un n ́umero dado.




infdesde :: Int -> [Int]
infdesde n = [n..]




--3. Generar una lista con los primeros n naturales.


primerosnnaturales :: Int -> [Int]
primerosnnaturales n = tomar n (infdesde 0)



--4 *. Retornar los primeros 5 elementos de una lista infinita de enteros positivos.


primeros5 :: [Int]
primeros5 = take 5 [0..]





--Utilizando funciones de alto orden resolver:


{-

5. Dada una lista de enteros, retornar sus cuadrados, es decir, dado [x0, x1, . . . , xn]
deberia retornar
-}

cuad :: Int -> Int
cuad x = x * x

cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados xs = map (cuad) xs




--6. Dado un entero positivo, retornar la lista de sus divisores.

divisor :: Int -> Int -> Bool
divisor x y = mod x y == 0


divisores' :: Int -> [Int]
divisores' n = filter (divisor n) [1..n]



{-
7. Dada una lista de naturales, obtener la lista que contenga solo los n ́umeros
primos de la lista original.-}


soloPrimos :: [Int] -> [Int]
soloPrimos [] = []
soloPrimos xs = filter (esPrimo) xs


{-8 *. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.-}

sumacuad :: [Int] -> Int 
sumacuad [] = 0
sumacuad xs = sum (map (cuad) xs)


{-9. Dada una lista de naturales, retornar la lista con sus sucesores.-}


sucesor :: Int -> Int
sucesor n = n+1

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores xs = map (sucesor) xs

{-10. Dada una lista de enteros, sumar todos sus elementos.-}

suma' :: (Num a) => a -> a -> a
suma' x y = x + y 

sumaenteros :: [Int] -> Int
sumaenteros [] = 0
sumaenteros xs = foldr (suma') 0 xs


sumaenteros' :: [Int] -> Int
sumaenteros' [] = 0
sumaenteros' xs = foldl (suma') 0 xs



{-11 *. Definir el factorial usando fold.-}

prod :: (Num a) => a -> a -> a
prod  x y = x * y


factorial' :: [Int] -> Int
factorial' [] = 1
factorial' xs = foldl (prod) 1 xs


factorial'' :: [Int] -> Int
factorial'' [] = 1
factorial'' xs = foldr (prod) 1 xs




{-13. Usando foldl o foldr definir una funci ́on tam::[a]->Int que devuelve la
cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
foldl evaluen diferente con los mismos parametros.-}



sumaa :: (Num a) =>  a -> a -> a
sumaa x y = y+1


tam :: (Num a) => [a] -> a
tam [] = 0
tam xs = foldr (sumaa) 0 xs


tam' :: (Num a) => [a] -> a
tam' [] = 0
tam' xs = foldl (sumaa) 0 xs




--UTILIZANDO LISTAS POR COMPRENSION 

{-14. Dada una lista de enteros, retornar sus sucesores.-}


suclistas :: [Int] -> [Int]
suclistas xs = [x+1 | x <- xs]


{-15 *. Dada una lista de naturales, retornar sus cuadrados.<-}



compcuad :: [Int] -> [Int]
compcuad xs = [cuad x | x <- xs]





{-16. Dada una lista de enteros, retornar los elementos pares que sean mayores a
10.-}


par :: Int -> Bool
par n = mod n 2 == 0

paresmayores10 :: [Int] -> [Int]
paresmayores10 xs = [x | x <- xs , par x && x >= 10]




{-17. Dado un entero, retornar sus divisores.-}


divisorescomp :: Int -> [Int]
divisorescomp n = [x | x <- [1..n] , mod n x == 0 ]




{-
18 *. Definir la funci ́on todosOcurrenEn :: Eq a => [a] -> [a] -> Bool

tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son ele-
mentos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True,

todosOcurrenEn [1,5,2,5] [5,2,4] = False-}



todosOcurrenEn :: (Eq a) => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [elem x ys | x <- xs]


{-19. Dado un natural n, retornar los n ́umeros primos comprendidos entre 2 y n.-}


primosentre2 :: Int -> [Int]
primosentre2 n = [x | x <-[2..n] , esPrimo x]



{-20. Dadas dos listas de naturales, retornar su producto cartesiano.-}

prodCartesiano :: [Int] ->  [Int] -> [(Int,Int)]
prodCartesiano xs ys = [(x,y) | x <- xs , y <- ys]



{-21 *. Dadas una lista y un elemento retornar el n ́umero de ocurrencias del
elemento x en la lista ys.-}

igual :: (Eq a) => a -> a -> Bool
igual x y = x == y


ocurrencias :: (Eq a ) => [a] -> a -> Int
ocurrencias xs e = sum [1 | x <- xs , igual x e  ]





{-
22. Escribir la funci ́on split2 :: [a] - > [([a],[a])], que dada una lista
xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo:
split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])].-}



split2 :: [a] -> [([a],[a])]
split2 xs = [(tirar n xs,tomar n xs) | n <-[0..length xs]]




{-
23 *. Definir una funci ́on que, dada una lista de enteros, devuelva la suma de
la suma de todos los segmentos iniciales.
Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10.-}




sumaseg :: [Int] -> Int
sumaseg xs = sum [sum (take n xs) | n <-[0..length xs]]