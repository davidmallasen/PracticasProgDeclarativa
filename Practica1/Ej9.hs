-- last xs = ultimo elemento de la lista no vacia xs
last' :: [a] -> a
last' [x]      = x
last' (_:xs)   = last' xs 

-- init xs = todos menos el ultimo elemento de la lista no vacia xs
init' :: [a] -> [a]
init' [x]      = []
init' (x:xs)   = x:(init' xs)

-- initLast xs = (init xs,last xs)
initLast' :: [a] -> ([a], a)
initLast' xs = (init' xs, last' xs)

-- concat xss = resultado de concatenar los elementos de la lista de listas xss 
concat' :: [[a]] -> [a]
concat' [xs]     = xs
concat' (xs:xss) = xs ++ (concat' xss)

-- take n xs = lista de los n primeros elementos de xs
take' :: Integral n => n -> [a] -> [a]
take' 0 xs     = []
take' n []     = []
take' n (x:xs) = x:(take' (n-1) xs) 

-- drop n xs = resultado de eliminar los n primeros elementos de xs
drop' :: Integral n => n -> [a] -> [a]
drop' 0 xs     = xs
drop' n []     = []
drop' n (x:xs) = drop' (n-1) xs

-- splitAt n xs = (take n xs, drop n xs)
splitAt' :: Integral n => n -> [a] -> ([a], [a])
splitAt' n xs = (take' n xs, drop' n xs)

-- reverse xs = inversa de la lista xs
reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x:xs)   = (reverse' xs) ++ [x]

-- deleteAllFrom y xs ys = resultado de eliminar las apariciones de x en la lista xs
deleteAllFrom :: Eq a => a -> [a] -> [a]
deleteAllFrom x xs = deleteAllFrom' x xs []
    where
        deleteAllFrom' y [] ys = ys
        deleteAllFrom' y (x:xs) ys
            |x == y     = deleteAllFrom' y xs  ys
            |otherwise  = deleteAllFrom' y xs $ ys ++ [x]

-- nub xs = resultado de eliminar los elementos repetidos de la lista xs
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x:nub' (deleteAllFrom x xs)

-- sort xs = resultado de ordenar la lista xs (usa diferentes metodos)
sort' :: Ord a => [a] -> [a]
sort' = mergeSort

mergeSort :: Ord a => [a] -> [a]
mergeSort []    = []
mergeSort [x]   = [x]
mergeSort xs    =  let spl = splitAt' ((length xs) `div` 2) xs
                    in merge (mergeSort (fst spl)) (mergeSort (snd spl))
                    where   merge xs [] = xs
                            merge [] ys = ys
                            merge (x:xs) (y:ys)
                                |x <= y     = x:(merge xs (y:ys))
                                |otherwise  = y:(merge (x:xs) ys)

-- and bs = resultado de hacer la conjuncion de todos los elementos de bs
and' :: Foldable t => t Bool -> Bool
and' bs = foldl (&&) True bs

-- or bs = resultado de hacer la disyuncion de todos los elementos de bs
or' :: Foldable t => t Bool -> Bool
or' bs = foldl (||) False bs

-- sum xs = resultado de sumar todos los elementos de xs
sum' :: (Foldable t, Num b) => t b -> b
sum' xs = foldr (+) 0 xs

-- product xs = resultado de multiplicar todos los elementos de xs
product' :: (Foldable t, Num b) => t b -> b
product' xs = foldr (*) 1 xs

-- length xs = longitud de xs
length' :: (Foldable t, Num b) => t p -> b
length' xs = foldl f 0 xs
    where f x y = x + 1

-- mean xs = media aritmetica de los elementos de xs
mean' :: Fractional p => [p] -> p
mean' [] = 0
mean' l = (sum' l) / (length' l)

-- lmedia xss = longitud media de los elementos de la lista de listas xss
lmedia :: (Fractional a, Foldable t) => [t p] -> a
lmedia xss = (sumaLong xss) / (length' xss)
    where   sumaLong [xs]       = length' xs
            sumaLong (xs:xss)   = (length' xs) + (sumaLong xss)