{-

















-}




{-map toma una función y una lista y aplica esa FUNCION a cada elemento de esa lista, 
produciendo una nueva lista. Vamos a ver su definición de tipo y como se define.

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs




filter es una función que toma un predicado 
(un predicado es una función que dice si algo es cierto o falso, o en nuestro caso, una función que devuelve un valor booleano)
y una lista y devuelve una lista con los elementos que satisfacen el predicado. 
La declaración de tipo y la implementación serían algo como:


filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

que basicamente si x cumple la propiedad p se introduce a la cabeza y en otro caso
se sigue con la recursion sin ese elemento ya que no satisface p

-}

-- map f (filter p xs) Ejemplo usando filter y map para hacer una lista por compresion 

{-
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)-}

--1. Generar una lista infinita de unos.



inf1 :: [Int]
inf1 = 1 : inf1


--2. Generar una lista infinita de naturales comenzando desde un n ́umero dado.


infdesde :: Int -> [Int]
infdesde n = n : infdesde (n+1)


--3. Generar una lista con los primeros n naturales.

nnaturales :: Int -> [Int]
nnaturales n = take n (infdesde 0)


--4 *. Retornar los primeros 5 elementos de una lista infinita de enteros positivos.




--Utilizando funciones de alto orden resolver:

--5. Dada una lista de enteros, retornar sus cuadrados.

cuad :: Int -> Int
cuad x = x * x


cuadrados :: [Int] -> [Int]
cuadrados xs = map cuad xs


--6. Dado un entero positivo, retornar sus divisores.

esDivisor :: Int -> Int -> Bool
esDivisor x y = mod x y == 0


divisores :: Int -> [Int]
divisores 0 = []
divisores n = filter (esDivisor n) [1..n]
---- map f (filter p xs)



--

--7. Dada una lista de naturales, obtener la lista que contenga solo los n ́umeros
--primos de la lista original.


esPrimo :: Int -> Bool
esPrimo n = tam (divisores n) == 2 

primos :: [Int] -> [Int]
primos [] = []
primos xs = filter esPrimo xs




--8 *. Dada una lista de naturales, retornar la suma de los cuadrados de la lista.

sumcuad :: [Int] -> Int
sumcuad [] = 0
sumcuad xs = sum(map cuad xs)


--9. Dada una lista de naturales, retornar la lista con sus sucesores.

sucesor :: Int -> Int
sucesor n = n + 1


sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores xs = map sucesor xs

--10. Dada una lista de enteros, sumar todos sus elementos.


suma :: Int -> Int -> Int 
suma x y = x + y 

sumatodos :: [Int] -> Int
sumatodos [] = 0 
sumatodos xs = foldl suma 0 xs



--11 *. Definir el factorial usando fold.

fact :: [Int] -> Int
fact xs = foldl (*) 1 xs


--12 *. Redefinir la funci ́on and tal que and xs se verifica si todos los elementos
--de xs son verdaderos. Por ejemplo: and[1 < 2, 2 < 3, 1/ = 0]− > True
--and[1 < 2, 2 < 3, 1 == 0]− > False


and' :: [Bool] -> Bool
and' [] = True 
and' (x:xs)
          | x == True = and' xs
          | otherwise = False 




--13. Usando foldl o foldr definir una funci ́on tam :: [a]− > Int que devuelve la
--cantidad de elementos de una lista dada. Dar un ejemplo en los cuales foldr y
--foldl evaluen diferente con los mismos parametros.



suma1 :: a -> Int -> Int 
suma1 x n = n+1

tam :: [a] -> Int 
tam xs = foldr suma1 0 xs





{-
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op acc [] = acc
foldl op acc (x:xs) = foldl op (op acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)-}











--Utilizando listas por comprensi ́on resolver:
--14. Dada una lista de enteros, retornar sus sucesores.





succcomp :: [Int] -> [Int]
succcomp xs = [succ x | x <- xs]


--15 *. Dada una lista de naturales, retornar sus cuadrados.


cuadrados' :: [Int] -> [Int]
cuadrados' xs = [x^2 | x <- xs]


--16. Dada una lista de enteros, retornar los elementos pares que sean mayores a 10.


esPar :: Int -> Bool
esPar n = mod n 2 == 0


mayor10 :: [Int] -> [Int]
mayor10 xs = [x | x <- xs , esPar x && x > 10]


--17. Dado un entero, retornar sus divisores.

divisores' :: Int -> [Int]
divisores' n = [x | x <- [1..n], mod n x == 0]



--18 *. Definir la funci ́on todosOcurrenEn :: Eq a => [a]− > [a]− > Bool tal
--que todosOcurrenEn xs ys se verifica si todos los elementos de xs son elementos
--de ys. Por ejemplo: todosOcurrenEn [1,5,2,5] [5,1,2,4] == True
--todosOcurrenEn [1,5,2,5] [5,2,4] == False
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
                where menores = [y | y <- xs, y <= x]
                      mayores = [y | y <- xs, y > x] 



todosOcurrenEn' :: (Eq a,Ord a) => [a] -> [a] -> Bool
todosOcurrenEn' xs ys = (quicksort xs) == (quicksort ys) 


todosOcurrenEn :: (Eq a ) =>[a] -> [a] -> Bool
todosOcurrenEn xs ys = and [elem x ys| x <-xs]




--19. Dado un natural n, retornar los n ́umeros primos comprendidos entre 2 y n.


primosdesde2 :: Int -> [Int]
primosdesde2 n = [x | x <- [2..n], esPrimo x]


--20. Dadas dos listas de naturales, retornar su producto cartesiano.

productocartesiano :: [Int] -> [Int] -> [(Int,Int)]
productocartesiano xs ys = [(x,y) | x <- xs, y <- ys]



--21 *. Dadas una lista y un elemento retornar el n ́umero de ocurrencias del
--elemento x en la lista ys.


esta :: (Eq a) => a -> a -> Bool
esta a x = a == x


ocurrencias :: (Eq a) => a -> [a] -> Int  -- Int la cantidad de ocurrencias de a en [a]
ocurrencias z xs = sum [1 | x <- xs , esta z x]




--22. Escribir la funci ́on split2 :: [a]− > [([a], [a])], que dada una lista xs, devuelve
--la lista con todas las formas de partir xs en dos.
--Por ejemplo: split2 [1,2,3] = [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]),([1,2,3],[])]


split2comp :: [a] -> [([a],[a])]
split2comp xs = [(take n xs,drop n xs)| n <-[0..length xs]]

split2 :: [a] -> Int -> [([a],[a])]
split2 xs 0 = [(drop 0 xs,take 0 xs)]
split2 xs n = [(drop n xs,take n xs)] ++ split2 xs (n-1)


split2' :: [a] -> [([a],[a])]
split2' xs = split2 xs (length xs) 


--23 *. Definir una funci ́on que, dada una lista de enteros, devuelva la suma de
--la suma de todos los segmentos iniciales.
--Por ejemplo: sumaSeg [1,2,3] = 0 + 1 + 3 + 6 = 10.

sumSeg' :: [Int] -> Int
sumSeg' xs = sum [sum (take n xs) | n <- [1..length xs]]

--24. Definir la lista infinita de los n ́umeros pares.

paresInfinita :: [Int]
paresInfinita = [2*x | x <- [0..]]





fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)









tamaño :: [a] -> Int
tamaño xs = foldr (\x -> (+) 1) 0 xs





mult :: Int -> Int -> Int 
mult x y = x * y 


multiplicar :: [Int] -> Int
multiplicar xs = foldr (mult) 1 xs


igualesAe :: (Eq a) => a -> a -> Bool
igualesAe x y = x == y 



