{-


.





Utilizando funciones de alto orden resolver:








12 *. Redefinir la función and tal que and xs se verifica si todos los ele-
mentos de xs son verdaderos. Por ejemplo: and [1<2, 2<3, 1/=0] = True,
and [1<2, 2<3, 1 == 0] = False.
13. Usando foldl o foldr definir una función tam::[a]->Int que devuelve la
cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
foldl evaluen diferente con los mismos parametros.





Utilizando listas por comprensión resolver:



17. Dado un entero, retornar sus divisores.

19. Dado un natural n, retornar los números primos comprendidos entre 2 y n.
20. Dadas dos listas de naturales, retornar su producto cartesiano.


23 *. Definir una función que, dada una lista de enteros, devuelva la suma de
la suma de todos los segmentos iniciales.
Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10.
24. Definir la lista infinita de los números pares.-}




{-1. Generar una lista infinita de unos.-}

inf1 = 1 : inf1

{-2. Generar una lista infinita de naturales comenzando desde un número dado.-}


infdesde :: Int -> [Int]
infdesde n = n : infdesde (n+1)


{-3. Generar una lista con los primeros n naturales.-}


primerosn :: Int -> [Int]
primerosn n = take n (infdesde 0)



{-4 *. Retornar los primeros 5 elementos de una lista infinita de enteros positivos-}
-- ya lo hice arriba hay que cambiar los parametros nada mas xd




{-5. Dada una lista de enteros, retornar sus cuadrados, es decir, dado [x0 , x1 , . . . , xn ]
deberia retornar [x20 , x21 , . . . , x2n ]-}



cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados xs = map (^2) xs



{-6. Dado un entero positivo, retornar la lista de sus divisores.-}


esDivisor :: Int -> Int -> Bool 
esDivisor a b = mod a b == 0 


divisores :: Int -> [Int]
divisores n = [x | x <- [1..n] , esDivisor n x ]




{-
7. Dada una lista de naturales, obtener la lista que contenga solo los números primos de la lista original.-}


esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2


soloPrimos :: [Int] -> [Int]
soloPrimos xs = filter (esPrimo) xs 


{-8 *. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.-}


sumCuad :: [Int] -> Int
sumCuad [] = 0
sumCuad xs = sum(cuadrados xs)

{-9. Dada una lista de naturales, retornar la lista con sus sucesores.-}


sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores xs = map (+1) xs



{-10. Dada una lista de enteros, sumar todos sus elementos.-}


sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria xs = sum xs



{-11 *. Definir el factorial usando fold.-}

fact1 :: [Int] -> Int
fact1 [] = 0
fact1 xs = foldr (*) 1 xs


fact2 :: [Int] -> Int
fact2 [] = 0
fact2 xs = foldl (*) 1 xs














{-14. Dada una lista de enteros, retornar sus sucesores.
15 *. Dada una lista de naturales, retornar sus cuadrados.-}


sucesores' :: [Int] -> [Int]
sucesores' xs = [x+1 | x <- xs ]



cuadrados' :: [Int] -> [Int]
cuadrados' [] = []
cuadrados' xs = [x*x | x <- xs]


{-
16. Dada una lista de enteros, retornar los elementos pares que sean mayores a 10-}


par :: Int -> Bool
par n = mod n 2 == 0 

paresMayores10 :: [Int] -> [Int]
paresMayores10 xs = [x | x <- xs, par x, x > 10]



{-18 *. Definir la función todosOcurrenEn :: Eq a => [a] -> [a] -> Bool
tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son ele-
mentos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True,
todosOcurrenEn [1,5,2,5] [5,2,4] = False-}



todosOcurrenEn :: (Eq a) => [a] -> [a] -> Bool
todosOcurrenEn xs ys = and [elem x ys | x <- xs]


{-21 *. Dadas una lista y un elemento retornar el número de ocurrencias del
elemento x en la lista ys.-}


ocurrencias :: (Eq a) => [a] -> a-> Int 
ocurrencias xs e = sum [1 | x <- xs, x == e ]



{-22. Escribir la función split2 :: [a] - > [([a],[a])], que dada una lista
xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo:
split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])].-}



split2 :: [a] -> [([a],[a])]
split2 xs = [(take n xs,drop n xs) | n <- [0..length xs ]]