maxDos :: Int -> Int -> Int
maxDos x y = if x > y then x else y 


maxTres :: Int -> Int -> Int -> Int 
maxTres x y z = maxDos x (maxDos y z )