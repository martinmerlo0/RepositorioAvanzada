main :: IO ()
main = return ()

esPar :: [Int] -> Bool
esPar (x:xs) = mod (head (reverse (x:xs))) 2 == 0