--Retorna la cabeza de la lista 


hd :: [a] -> a
--hd [] = 
hd (x:xs) = x


--retorna la lista sin la cabeza

tl :: [a] -> [a]
tl (x:xs) = xs 



--last retorna el ultimo elemento de la lista 


last' :: [a] -> a 
last' [x] = x
last' (x:xs) = last' xs 


-- retorna todos los elementos menos el ultimo 


init' :: [a] -> [a]
init' [x] = [] 
init' (x:xs) = x : init'(xs)