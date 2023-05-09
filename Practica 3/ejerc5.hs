--5 *. Define una funci ́on que, dado un n ́umero natural n en su representaci ́on
--binaria, decida si n es par o no.

-- 0101 impar 1010 par 
par :: Int -> Bool
par 0 = True 
par 1 = False 
par bin = if mod bin 2 == 0 then True else False