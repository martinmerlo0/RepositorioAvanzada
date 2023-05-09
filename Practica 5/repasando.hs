{-NOTAs: Les recomendamos, antes de comenzar a resolver los ejercicios, repasar
la teor ́ıa.
Los ejercicios marcados con * son para resolver en su casa.
1. Generar una lista infinita de unos.
2. Generar una lista infinita de naturales comenzando desde un n ́umero dado.
3. Generar una lista con los primeros n naturales.
4 *. Retornar los primeros 5 elementos de una lista infinita de enteros positivos.



Utilizando funciones de alto orden resolver:
, es decir, dado [x0, x1, . . . , xn]
deberia retornar [x
2
0
, x2
1
, . . . , x2
n
]








12 *. Redefinir la funci ́on and tal que and xs se verifica si todos los ele-
mentos de xs son verdaderos. Por ejemplo: and [1<2, 2<3, 1/=0] = True,

and [1<2, 2<3, 1 == 0] = False.











16. Dada una lista de enteros, retornar los elementos pares que sean mayores a
10.
17. Dado un entero, retornar sus divisores.
18 *. Definir la funci ́on todosOcurrenEn :: Eq a => [a] -> [a] -> Bool

tal que todosOcurrenEn xs ys se verifica si todos los elementos de xs son ele-
mentos de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] = True,

todosOcurrenEn [1,5,2,5] [5,2,4] = False
19. Dado un natural n, retornar los n ́umeros primos comprendidos entre 2 y n.
20. Dadas dos listas de naturales, retornar su producto cartesiano.
21 *. Dadas una lista y un elemento retornar el n ́umero de ocurrencias del
elemento x en la lista ys.
22. Escribir la funci ́on split2 :: [a] - > [([a],[a])], que dada una lista
xs, devuelve la lista con todas las formas de partir xs en dos. Por ejemplo:
split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])].
23 *. Definir una funci ́on que, dada una lista de enteros, devuelva la suma de
la suma de todos los segmentos iniciales.
Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10.
24. Definir la lista infinita de los n ́umeros pares.-}


--5. Dada una lista de enteros, retornar sus cuadrados


cuad :: Int -> Int
cuad x = x * x


cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados xs = map cuad xs



--6. Dado un entero positivo, retornar la lista de sus divisores.


esDivisor :: Int -> Int -> Bool 
esDivisor n m = mod n m == 0


divisores :: Int -> [Int]
divisores n = filter (esDivisor n) [1..n]


--7. Dada una lista de naturales, obtener la lista que contenga solo los n ́umeros primos de la lista original.


esPrimo :: Int -> Bool
esPrimo n = length (divisores n) == 2


primos :: [Int] -> [Int]
primos [] = []
primos xs = filter esPrimo xs

--8 *. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.


sumCuad :: [Int] -> Int 
sumCuad [] = 0
sumCuad xs = sum (cuadrados xs)



--9. Dada una lista de naturales, retornar la lista con sus sucesores.


suc :: Int -> Int
suc n = n + 1


sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores xs = map suc xs



--10. Dada una lista de enteros, sumar todos sus elementos.


sum1 :: [Int] -> Int
sum1 [] = 0
sum1 xs = foldr (+) 0 xs


sum2 :: [Int] -> Int 
sum2 [] = 0 
sum2 xs = foldl (+) 0 xs



--11 *. Definir el factorial usando fold.


fact :: [Int] -> Int -- La lista contiene el factorial de [1..n]
fact [] = 1
fact [0] = 1
fact xs = foldl (*) 1 xs




--13. Usando foldl o foldr definir una funci ́on tam::[a]->Int que devuelve la
--cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
--foldl evaluen diferente con los mismos parametros.



--tam :: [a] -> Int
--tam lista = foldr (\_ acc -> acc + 1) 0 lista



--Utilizando listas por comprensi ́on resolver:
--14. Dada una lista de enteros, retornar sus sucesores.



sucesores' :: [Int] -> [Int]
sucesores' xs = [x+1 | x <- xs]


--15 *. Dada una lista de naturales, retornar sus cuadrados.


cuadrados' :: [Int] -> [Int]
cuadrados' xs = [cuad x| x <- xs]


sumar1 ::(Num a)=> a -> Int -> Int
sumar1 x n = n+1



tam :: (Num a) =>[a] -> Int
tam xs = foldr sumar1 0 xs


tam' :: (Num a) => [a] -> Int 
tam' xs =  foldl sumar1' 0 xs

sumar1' :: Int -> a -> Int
sumar1' n x = n+1




{-foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op acc [] = acc
foldl op acc (x:xs) = foldl op (op acc x) xs


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)



-}