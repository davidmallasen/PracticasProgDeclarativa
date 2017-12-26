-- PROGRAMACION DECLARATIVA CURSO 17/18
-- PRACTICA INDIVIDUAL: LOGICA PROPOSICIONAL
-- David Mallasen Quintana (DG)

-- f1 = (¬p => (p => (q /\ ¬q)))    Tautologia pq
f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))
-- f2 = (p /\ (¬q => ¬p))           True sii todos pq True
f2 = Y (V "p") (Si (No (V "q")) (No (V "p")))
-- f3 = (p /\ (q /\ (¬q \/ r)))     True sii todos pqr True
f3 = Y (V "p") (Y (V "q") (O (No (V "q")) (V "r")))
-- f4 = (¬(p \/ q) => (p => r))     Tautologia pqr
f4 = Si (No (O (V "p") (V "q"))) (Si (V "p") (V "r"))
-- f5 = ((p /\ q) /\ (p => ¬q))     Falacia pq
f5 = Y (Y (V "p") (V "q")) (Si (V "p") (No (V "q")))
-- f6 = ((r \/ p) /\ ¬(q \/ p))     True sii pq False, r True
f6 = Y (O (V "r") (V "p")) (No (O (V "q") (V "p")))

type Var = String -- nombres de variables

data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp 
    deriving Read

-- Declarar FProp como instancia de la clase Eq, haciendo que la igualdad entre formulas coincida
    -- con la igualdad estructural (componente a componente), salvo por el hecho de que el orden en
    -- conjunciones o negaciones no importe.
instance Eq FProp where
    -- (==) :: a -> a -> Bool
    V x     == V x'         = x == x'
    No x    == No x'        = x == x'
    Y x y   == Y x' y'      = ((x == x') && (y == y')) || ((x == y') && (y == x'))
    O x y   == O x' y'      = ((x == x') && (y == y')) || ((x == y') && (y == x'))
    Si x y  == Si x' y'     = (x == x') && (y == y')
    Sii x y == Sii x' y'    = (x == x') && (y == y')
    x       == x'           = False

-- Declarar FProp como instancia de la clase Ord, de modo que una formula f sea menor que otra f'
    -- si f' es consecuencia logica de f.
instance Ord FProp where
    -- (<=) :: a -> a -> Bool
    x <= y = consecuencia x y

-- Declarar FProp como instancia de la clase Show de modo que la visualizacion de una formula sea
    -- algo mas legible que lo que proporciona directamente deriving Show.
instance Show FProp where
    -- show :: a -> String
    show (V x)      = x
    show (No x)     = "¬" ++ show x
    show (Y x y)    = "(" ++ show x ++ " /\\ " ++ show y ++ ")"
    show (O x y)    = "(" ++ show x ++ " \\/ " ++ show y ++ ")"    
    show (Si x y)   = "(" ++ show x ++ " => " ++ show y ++ ")"
    show (Sii x y)  = "(" ++ show x ++ " <=> " ++ show y ++ ")"

-- Vars: vars f calcula una lista con los nombres de las variables proposicionales que hay en f,
    -- sin repeticiones (aunque el orden es irrelevante).
vars :: FProp -> [Var]
vars f =  nub $ vars' f
    where
        vars' (V x) = [x]
        vars' (No x) = vars' x
        vars' (Y x y) = (vars' x) ++ (vars' y)
        vars' (O x y) = (vars' x) ++ (vars' y)
        vars' (Si x y) = (vars' x) ++ (vars' y)
        vars' (Sii x y) = (vars' x) ++ (vars' y)

-- Tautologia: tautologia f reconoce si f es una tautologia o no, es decir, si es cierta para
    -- valores de verdad cualesquiera (True o False) de sus variables proposicionales.
tautologia :: FProp -> Bool
tautologia f = and $ map (evalua f) (partes (vars f))

-- Satisfactible: satisfactible f reconoce si f es satisfactible o no, es decir, si es cierta
    -- para algunos valores de verdad de sus variables proposicionales.
satisfactible :: FProp -> Bool
satisfactible f = or $ map (evalua f) (partes (vars f))

-- Consecuencia: consecuencia f1 f2 reconoce si una formula f1 es consecuencia logica de otra f2,
    -- es decir, si para valores de verdad cualesquiera de las variables proposicionales, cuando f2
    -- es cierta, f1 lo es tambien.
consecuencia :: FProp -> FProp -> Bool
consecuencia f1 f2 = tautologia (Si f2 f1)

-- Equivalente: equivalente f1 f2 reconoce si f1 y f2 son logicamente equivalentes, es decir, si
    -- para valores de verdad cualesquiera de las variables proposicionales, f1 es cierta si y solo
    -- si f2 lo es tambien.
equivalente :: FProp -> FProp -> Bool
equivalente f1 f2 = tautologia (Sii f1 f2)

-- Consecuencias: dada una lista fs de formulas, consecuencias fs es una lista con cada formula f
    -- de fs emparejada con la lista de aquellas formulas de fs que son consecuencia logica de f.
consecuencias :: [FProp] -> [(FProp, [FProp])]
consecuencias fs = [(f, [f' | f' <- fs, consecuencia f' f]) | f <- fs]

-- Equivalentes: dada una lista fs de formulas, equivalentes fs es el conjunto cociente de fs por
    -- la relacion de equivalencia logica, es decir, es una particion de fs en sublistas, cada una
    -- de las cuales esta formada por formulas de fs equivalentes entre si.
equivalentes :: [FProp] -> [[FProp]]
equivalentes [] = []
equivalentes (f:fs) =
    let (eqs, rst) = equiv f (f:fs) ([], []) where
        equiv z [] (xs, ys) = (xs, ys)
        equiv z (f:fs) (xs, ys)
            | equivalente z f = equiv z fs (f:xs, ys)
            | otherwise = equiv z fs (xs, f:ys)
    in eqs:(equivalentes rst)


-- ========== INTERACCION USUARIO ==========

-- Menu: Pequenya interaccion con el usuario. Pide al usuario que introduzca las formulas, se pide
    -- en un sencillo menu que quiere hacer con ellas y se muestra el resultado de lo pedido.
menu =
    do  putStrLn "=== OPERACIONES LOGICA PROPOSICIONAL ==="
        fs <- getFormulas
        putStrLn "Lista de opciones: "
        putStrLn "1 - Listar las variables de una formula."
        putStrLn "2 - Reconocer si una formula es una tautologia."
        putStrLn "3 - Reconocer si una formula es satisfactible."
        putStrLn "4 - Reconocer si una formula f1 es consecuencia de otra f2."
        putStrLn "5 - Reconocer si dos formulas son equivalentes."
        putStrLn "6 - Listar cada formula junto con aquellas que son consecuencia logica suya."
        putStrLn "7 - Devolver el conjunto cociente por la relacion de equivalencia logica."
        putStrLn "0 - Salir."
        putStr "Introduzca la opcion deseada: "
        opc <- getInt
        procesarMenu opc fs

procesarMenu 0 fs = putStrLn "Saliendo..."
procesarMenu opc [] = putStrLn "Error. Escriba al menos una formula en la lista!"
procesarMenu opc (f:fs)
    | opc == 1 = print $ vars f
    | opc == 2 = print $ tautologia f
    | opc == 3 = print $ satisfactible f
    | opc == 4 = print $ consecuencia f (head fs)
    | opc == 5 = print $ equivalente f (head fs)
    | opc == 6 = print $ consecuencias (f:fs)
    | opc == 7 = print $ equivalentes (f:fs)


-- ========== FUNCIONES AUXILIARES ==========

-- deleteAllFrom x xs = resultado de eliminar las apariciones de x en la lista xs
deleteAllFrom :: Eq a => a -> [a] -> [a]
deleteAllFrom x xs = deleteAllFrom' x xs []
    where
        deleteAllFrom' y [] ys = ys
        deleteAllFrom' y (x:xs) ys
            | x == y    = deleteAllFrom' y xs  ys
            | otherwise = deleteAllFrom' y xs $ ys ++ [x]

-- nub xs = resultado de eliminar los elementos repetidos de la lista xs
nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x:nub (deleteAllFrom x xs)

-- partes xs = lista de todas las partes (subconjuntos) de xs
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x:xs) = 
    let p = partes xs in 
        p ++ [x:ps | ps <- p]

-- evalua f xs = resultado de evaluar f sobre xs. Si una variable esta en xs significa que su valor
    -- es True. Si no esta, su valor es False.
evalua :: FProp -> [Var] -> Bool
evalua (V x) xs       = elem x xs
evalua (No x) xs      = not (evalua x xs)
evalua (Y x y) xs     = and [(evalua x xs), (evalua y xs)]
evalua (O x y) xs     = or [(evalua x xs), (evalua y xs)]
evalua (Si x y) xs    = if (evalua x xs) then (evalua y xs) else True
evalua (Sii x y) xs   = (evalua x xs) == (evalua y xs)

-- Pide al usuario una lista de formulas y las devuelve
getFormulas :: IO [FProp]
getFormulas = 
    do  putStr "Escribe una lista de formulas: "
        f <- getLine
        return (read f::[FProp])

-- Lee un entero. Funcion extraida de las diapositivas de clase.
getInt :: IO Int 
getInt = do line <- getLine
            return (read line::Int)