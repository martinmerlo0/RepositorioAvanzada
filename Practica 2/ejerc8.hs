--8. Defina una funci ́on que dado un n ́umero natural, decida si el mismo es primo o no.



{-prim' :: Int -> Int -> Bool
prim' x 1 = False
prim' x y = mod x y     || prim'(x (y-1))

prim :: Int -> Bool
prim 1 = False
prim 2 = True 
prim x = not(prim' x (x-1))-}


divisores :: Int -> [Int]
divisores n = [y | y <- [1..n] , (mod n y) == 0]



primo :: Int -> Bool
primo n = if length (divisores n) == 2 then True else False