esPar :: Int -> Bool
esPar n = mod n 2 == 0



pares :: Int -> [Int]
pares n = [x | x <- [0..n], esPar x]



impares :: Int -> [Int]
impares n = [x | x <- [0..n], not(esPar x)]



sumaindices :: [Int] -> [Int] -> [Int]
sumaindices [] ys = ys
sumaindices xs [] = xs
sumaindices (x:xs) (y:ys) = (x+y) : sumaindices xs ys 



sumaindices' :: Int -> [Int]
sumaindices' n = sumaindices (pares n) (impares n)