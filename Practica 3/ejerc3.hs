--3. Define una funci ́on que, recursivamente y s ́olo utilizando adici ́on y multipli-
--caci ́on, calcule, dado un natural n, el n ́umero 2n.



potencia2 :: Int -> Int 
potencia2 n 
    | n == 0 = 1 
    | n == 1 = 2
    | otherwise = 2 * potencia2 (n-1)