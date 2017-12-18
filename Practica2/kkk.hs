filter2 :: [a] -> (a -> Bool) -> (a -> Bool) -> ([a], [a])
filter2 xs p q =  ((filter p xs), (filter q xs))

filters :: [a] -> [a -> Bool] -> [[a]]
filters xs ps = [filter p xs | p <- ps]

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = filter2 xs p (not.p)

span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = let ys = takeWhile p xs in (ys, drop (length ys) xs)

