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
-- -----------------------------------------------
-- borra x xs = resultado de eliminar las apariciones de x en la lista xs

deleteAllFrom y [] ys = ys
deleteAllFrom y (x:xs) ys
   |x == y     = deleteAllFrom y xs ys
   |otherwise  = deleteAllFrom y xs (ys ++ x)

-- nub xs = resultado de eliminar los elementos repetidos de la lista xs
-- nub' (x:xs) =

delete' x xs = [y | y <- xs, y /= x]


