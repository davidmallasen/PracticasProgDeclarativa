data Arbol a = N a [Arbol a] deriving (Eq)

instance Ord a => Ord (Arbol a) where
    N x y <= N s t = x < s || x == s && y <= t

instance Show a => Show (Arbol a) where
    show (N x []) = "(" ++ show x ++ ")"
    show (N x y) = "(" ++ show x ++ " " ++ (concat (map show y)) ++ ")"


listaArbol :: Arbol t -> [t]
listaArbol (N x []) = [x]
listaArbol (N x y) = [x]++(concat (map listaArbol y))

listaHojas :: Arbol t -> [t]
listaHojas (N x []) = [x]
listaHojas (N x y) = concat $ map listaHojas y

listaNodos :: Arbol t ->[t]
listaNodos (N x []) = []
listaNodos (N x y) = [x]++(concat (map listaNodos y))

repMax :: Ord t => Arbol t -> Arbol t
repMax (N x y) = repVal (maximum (listaArbol (N x y))) (N x y)  where
    repVal m (N x y) = N m (map (repVal m) y) 
