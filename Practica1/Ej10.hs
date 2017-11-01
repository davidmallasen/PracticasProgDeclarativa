reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse xs) ++ [x]

reverse'' :: [a] -> [a]
reverse'' xs = revAux xs []
    where
    revAux [] xs = xs
    revAux (x:xs) ys = revAux xs (x:ys)

--TARDA MUCHO MÁS CON REVERSE'', POR QUE????
