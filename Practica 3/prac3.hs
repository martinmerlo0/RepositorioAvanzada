--1. Define una funci ́on que, dadas dos listas ys y xs de naturales ordenadas,
--retorne el merge de estas listas, es decir, la lista ordenada compuesta por los
--elementos de ys y xs.



merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge xs ys = ordena (xs ++ ys) 




--2. Define una funci´on que, dada una lista de naturales, la ordene.


quicksort :: (Ord a)=>[a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort menores ++ [x] ++ quicksort mayores
    where menores = [y | y <- xs, y <= x]
          mayores = [y | y <- xs, y >  x]



burbujeo :: (Ord a)=>[a] -> [a]
burbujeo [] = []
burbujeo [x] = [x]
burbujeo (x:y:xs) 
    | x >= y = y:burbujeo(x:xs)
    | otherwise = x:burbujeo(y:xs)

ordena :: (Ord a) => [a] -> [a]
ordena [] = []
ordena xs = ordena (init (burbujeo xs)) ++ [last (burbujeo xs)]


{-
Buscar el mínimo elemento de la lista
Intercambiarlo con el primero
Buscar el siguiente mínimo en el resto de la lista
Intercambiarlo con el segundo
Y en general:

Buscar el mínimo elemento entre una posición i y el final de la lista
Intercambiar el mínimo con el elemento de la posición i-}

minimo :: (Ord a) => a -> a -> a -- que a seria el minimo de estos 2
minimo x y = if x < y then x else y 


minimolista ::(Ord a)=> [a] -> a
minimolista [x] = x
minimolista (x:xs) = minimo x (minimolista xs)


elim :: (Eq a ) => [a] -> a -> [a]
elim [] y = []
elim [x] y = if x == y then [] else [x]
elim (x:xs) y = if x == y then xs else x : elim xs y


selectionsort :: (Ord a)=> [a] -> [a]
selectionsort [] = []
selectionsort [x] = [x]
selectionsort (x:xs) = minimolista (x:xs) : selectionsort(elim (x:xs) (minimolista(x:xs)))


--4. Define una funci ́on que, dado un n ́umero natural n, retorne su representaci ́on
--binaria como secuencia de bits.


binaria :: Int -> [Int]
binaria 0 = [0]
binaria n = mod n 2 : binaria (div n 2)

binaria' :: Int -> [Int]
binaria' n = reverse (binaria n)



--5 *. Define una funci ́on que, dado un n ́umero natural n en su representaci ́on
--binaria, decida si n es par o no.


parbin :: [Int] -> Bool
parbin [] = False
parbin xs = if last xs == 1 then False else True 



--6. Define la funci ́on que retorne la distancia de Hamming: dadas dos listas es el
--n ́umero de posiciones en que los correspondientes elementos son distintos. Por
--ejemplo: distanciaH ”roma””camino”− > 3
--distanciaH ”romano””rama”− > 1



hamming :: [Char] -> [Char] -> Int
hamming xs [] = 0
hamming [] ys = 0
hamming (x:xs) (y:ys) = if x == y then hamming xs ys else 1 + hamming xs ys 



--7. Define la funci ́on que, dado un n ́umero natural, decida si el mismo es un
--cuadrado perfecto o no.


--Un cuadrado perfecto es el resultado de multiplicar un número por sí mismo.

--x · x = a²


cuadperfect :: Int -> Bool 
cuadperfect n = floor(sqrt(fromIntegral n))^2 == n 


--8. Define la funci ́on repetidos de forma tal que dado un elemento z y un entero
--n; z aparece n veces.


repetidos :: a -> Int -> [a]
repetidos _ 0 = []
repetidos a n = a : repetidos a (n-1)



--9. Define la funci ́on nelem tal que nelem xs n es elemento en ́esimo de xs,
--empezando a numerar desde el 0. Por ejemplo:
--nelem [1, 3, 2, 4, 9, 7]3− > 4



nelem :: [a] -> Int -> a
nelem (x:xs) n = if n == 0 then x else nelem xs (n-1) 
--nelem (x:xs) n | n == 0 = x
            --   | otherwise = nelem xs (n-1)




--10 *. Define la funci ́on posicionesC tal que posicionesC xs c es la lista de la
--posiciones del caracter c en la cadena xs. Por ejemplo:
--posicionesC ”Catamarca”
--0a
--0− > [1, 3, 5, 8]

posicionesC :: [Char] -> Char -> Int -> [Int]
posicionesC [] _ _= []
posicionesC (x:xs) c n = if x == c then n : posicionesC xs c (n+1) else posicionesC xs c (n+1)




--11. Define la funci ́on compact, dada una lista retorna la lista sin los elementos
--repetidos consecutivos. Por ejemplo: compact [1, 3, 3, 5, 8, 3] = [1, 3, 5, 8, 3]



compact :: [Int] -> [Int]
compact [] = []
compact [x] = [x]
compact (x:y:ys) = if x == y then compact (y:ys) else x : compact (y:ys)




reverse' :: (Eq a)=>[a] -> [a]
reverse' [] = []
reverse' (x:xs) = last (x:xs) : reverse'(elim (x:xs) (last(x:xs))) 


--minimolista (x:xs) : selectionsort(elim (x:xs) (minimolista(x:xs)))
