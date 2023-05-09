--4. Defina las siguientes operaciones sobre listas (vistas en el te ́orico): concate-
--nar, tomar, tirar y C.


conc :: [a] -> [a] -> [a]
conc xs [] = xs
conc [] ys = ys 
conc (x:xs) ys = x : conc xs ys 


tirar :: [a] -> Int -> [a]
tirar [] n = []
tirar (x:xs) 0 = (x:xs)
tirar (x:xs) n = tirar xs (n-1) 



tomar :: Int -> [a] -> [a]
tomar _ [] = []
tomar 0 xs = [] 
tomar n (x:xs) = x : tomar (n-1) xs


concFinal :: [a] -> a -> [a]
concFinal [] n = [n]
concFinal [x] n = [x,n]
concFinal (x:xs) n = x : concFinal xs n 