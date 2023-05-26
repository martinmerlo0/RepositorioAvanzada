
{-
Ejercicio 3. Especificar y derivar la siguiente funci ́on:
f xs dice si todos los elementos
-}


iguales :: (Eq a) => [a] -> Bool
iguales [] = True
iguales (x:xs) = iguales' x xs

iguales' :: (Eq a) => a -> [a] -> Bool
iguales' z [] = True 
iguales' z (x:xs) = (z==x) && iguales' z xs








{-
* Ejercicio 5. Derivar una funci ́on que dada una lista determina si los elemen-
tos est ́an ordenados de forma creciente.
-}

creciente :: (Ord a) => [a] -> Bool
creciente [] = True
creciente [x] = True
creciente (x:xs) = x <= head xs && creciente xs








{-
Ejercicio 6. Sea m : [Num] → [Num] una funci ́on que devuelve el m ́ınimo de
una lista dada. Especificar y derivar m.
-}

minimo :: [Int] -> Int
minimo [] = 100
minimo (x:xs) = min x (minimo xs)








{-
Ejercicio 7. Especificar y derivar una funci ́on que dada una lista determine si
existe un elemento en ella que sea igual a la suma del resto de los elementos de
la lista.
-}

elemSum :: (Eq a ,Num a) => [a] -> Bool
elemSum [] = False
elemSum (x:xs) = elemSum' 0 (x:xs)


elemSum' :: (Eq a,Num a) => a -> [a] -> Bool
elemSum' n [] = False
elemSum' n (x:xs) = (x == (n + sum xs)) || elemSum' (n+x) xs



{-
Ejercicio 9. Derivar un programa usando la siguiente especificaci ́on:
P xs.ys = (∃as, bs :: ys = as + +xs + +bs),
que dadas dos listas determina si la primera es subsegmento de la segunda.
-}

subsegmento :: (Eq a)=>[a] -> [a] -> Bool 
subsegmento [] ys = True 
subsegmento (x:xs) [] = False
subsegmento (x:xs) (y:ys) = (y == x) && q xs ys || subsegmento (x:xs) ys



q :: (Eq a) => [a] -> [a] -> Bool
q [] ys = True
q (x:xs) [] = False 
q (x:xs) (y:ys) = (y == x) && q xs ys 