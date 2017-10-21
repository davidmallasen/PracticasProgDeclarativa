f x y = 2 * x - y * x

g x = f (f 2 x) (f x 1)

h x y z = f (f (x + (*) 2  y) (g 3)) (5 - (g z) - y)

i x y = if x >= y && y > 0 then (-) x y
        else if 0 < x && x < y then 0
        else y - x

i' x y  | x >= y && y > 0 = x - y
        | x > 0 && y > x = 0
        | otherwise = y-x
