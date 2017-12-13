class Medible a where
	tamanyo::a -> Int

instance Medible Bool where
	tamanyo True = 1
	tamanyo False = -1

instance Medible a => Medible [a] where
	tamanyo [] = 0
	tamanyo (x:xs) = tamanyo x + tamanyo xs

instance Medible a b => Medible (a,b) where
	tamanyo (a,b) = tamanyo a + tamanyo b