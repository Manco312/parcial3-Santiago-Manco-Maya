oddNumber :: Integral t => t -> t -> Bool
oddNumber x y =
    if x == 2 
        then True
    else if mod x y == 0
        then False
    else if y == (x-1)
        then True
    else 
        oddNumber x (y+1)

numerosEntreDosYElSiguienteValor :: Integral t => t -> [t]
numerosEntreDosYElSiguienteValor x =
    if x >= 2
        then filter (\y -> oddNumber y 2) [2..x]
    else
        [0]