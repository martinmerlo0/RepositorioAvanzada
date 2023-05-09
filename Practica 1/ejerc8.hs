--8. Escriba una funci ́on que dado un n ́umero retorne la lista de sus digitos.
main :: IO ()
main = return ()

solodigitos :: Int -> [Int]
solodigitos x = reverse (solodigitos2 x)

solodigitos2 :: Int -> [Int]
solodigitos2 x = if x < 10 then [x] else mod x 10 : solodigitos2 (div x 10)

solodigitos3 :: Int -> [Int]
solodigitos3 0 = []
solodigitos3 x = if x < 10 then [x] else mod x 10 : solodigitos3 (div x 10)