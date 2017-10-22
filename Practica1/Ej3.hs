digito :: [Int] -> Int
digito x = digito' x 0 where
	digito' 0 n = n
	digito' x n = digito' (x `div` 10) (n + 1)

reduccion :: Integral a => a -> a
reduccion x | x < 0 = reduccion (-x)
			| otherwise = reduccion' x  0
	where
		reduccion' x ac | x == 0 && ac > 9 = reduccion' ac 0
						| x == 0 = ac
						| otherwise = reduccion' (x `div` 10) (ac + x `mod` 10 )

perm :: Integral a => a -> a
perm 1 = 1
perm n = n * (perm (n - 1))

var :: Integral a => a -> a -> a
var n m | n == m + 1 = n
        | n == m     = 1
        |otherwise   = n * (var (n-1) m)


comb :: Integral a => a -> a -> a
comb n m = (var n m) `div` perm (n-m)