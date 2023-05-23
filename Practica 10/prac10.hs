
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