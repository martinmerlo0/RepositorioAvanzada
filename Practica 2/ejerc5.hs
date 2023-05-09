--5. Defina una funci ́on abs: Int -> Int que calcula el valor absoluto de un
--n ́umero.


absoluto :: Integer -> Integer 
absoluto n = if n >= 0 then n else (-1)*(n)