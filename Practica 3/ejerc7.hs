--7. Define la funci ́on que, dado un n ́umero natural, decida si el mismo es un
--cuadrado perfecto o no.



cuadradop :: Float  -> Bool
cuadradop num = mod sqrt(num) 1 == 0