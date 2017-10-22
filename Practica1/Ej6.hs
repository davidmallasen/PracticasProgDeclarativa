f x y z = x + g y z
 where g y z = x+y


h x y z
 |gs y z==0 = 1
 |otherwise = x

gs y z = y+z-y-z
bot = undefined