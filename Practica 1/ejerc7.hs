main :: IO ()
main = return ()

multiplo' :: [Int] -> Int -> Bool -- La voy a hacer mas generica a la solucion para cualquier numero
multiplo' xs n = (mod (sum xs) n == 0)