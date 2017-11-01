-- Calcular la lista de divisores de un numero
listaDiv :: Integral a => a -> [a]
listaDiv n = lDivAux n (n-1) [n]
    where lDivAux n m xs
            | m == 0            = xs
            | n `rem` m == 0    = lDivAux n (m-1) (m:xs)
            | otherwise         = lDivAux n (m-1) xs

-- Calcular la lista de digitos de un numero entero
listaDig :: Integral a => a -> [a]
listaDig 0 = [0]
listaDig n = lDigAux n [] 
    where   lDigAux 0 xs = xs
            lDigAux n xs = lDigAux (div n 10) ((rem n 10):xs)

-- Calcular el numero de digitos de un numero entero
nDigit :: Integral a => a -> a
nDigit 0 = 0
nDigit x = nDigit' x 0 where
    nDigit' 0 n = n
    nDigit' x n = nDigit' (x `div` 10) (n + 1)

-- Saber si todos los dgitos de un numero entero son pares
digitPares n
    | n == 0 = True
    | n `rem` 2 == 1 = False
    | otherwise = digitPares (n `div` 10)