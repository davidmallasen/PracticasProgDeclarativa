type Var = String -- nombres de variables

data FProp = V Var | No FProp | Y FProp FProp | O FProp FProp | Si FProp FProp | Sii FProp FProp

-- Declarar FProp como instancia de la clase Eq, haciendo que la igualdad entre formulas coincida
    -- con la igualdad estructural (componente a componente), salvo por el hecho de que el orden en
    -- conjunciones o negaciones no importe.
instance Eq FProp where
    -- (==) :: a -> a -> Bool

-- Declarar FProp como instancia de la clase Ord, de modo que una formula f sea menor que otra f' si
    -- f' es consecuencia logica de f.
instance Ord FProp where
    -- (<) :: a -> a -> Bool

-- Declarar FProp como instancia de la clase Show de modo que la visualizacion de una formula sea
    -- algo mas legible que lo que proporciona directamente deriving Show. Por ejemplo, de modo que
    -- al evaluar f1 el resultado se vea como ¬p -> (p -> (q /\ ¬q)).
instance Show FProp where
    -- show :: a -> String

f1 = Si (No (V "p")) (Si (V "p") (Y (V "q") (No (V "q"))))  --Tautologia
f2 = Y (V "p") (Si (No (V "q")) (No (V "p")))   --Tautologia
f3 = Y (V "p") (Y (V "q") (O (No (V "q")) (V "r")))   --True sii todos True

-- Vars: vars f calcula una lista con los nombres de las variables proposicionales que hay en f,
    -- sin repeticiones (aunque el orden es irrelevante). Por ejemplo, vars f1 debe evaluarse
    -- a ["p", "q"].
vars :: FProp -> [Var]

--OJO: MIRAR LA FUNCION ELEM

-- Tautologia: tautologia f reconoce si f es una tautologia o no, es decir, si es cierta para
    -- valores de verdad cualesquiera (True o False) de sus variables proposicionales.
tautologia :: FProp -> Bool

-- Satisfactible: satisfactible f reconoce si f es satisfactible o no, es decir, si es cierta
    -- para algunos valores de verdad de sus variables proposicionales.
satisfactible :: FProp -> Bool

-- Consecuencia: consecuencia f1 f2 reconoce si una formula f1 es consecuencia logica de otra f2,
    -- es decir, si para valores de verdad cualesquiera de las variables proposicionales, cuando f2
    -- es cierta, f1 lo es tambien.
consecuencia :: FProp -> FProp -> Bool

-- Equivalente: equivalente f1 f2 reconoce si f1 y f2 son logicamente equivalentes, es decir, si
    -- para valores de verdad cualesquiera de las variables proposicionales, f1 es cierta si y solo
    -- si f2 lo es tambien.
equivalente :: FProp -> FProp -> Bool

-- Consecuencias: dada una lista fs de formulas, consecuencias fs es una lista con cada formula f de
    -- fs emparejada con la lista de aquellas formulas de fs que son consecuencia logica de f.
consecuencias :: [FProp] -> [(FProp, [FProp])]

-- Equivalentes: dada una lista fs de formulas, equivalentes fs es el conjunto cociente de fs por la
    -- relacion de equivalencia logica, es decir, es una particion de fs en sublistas, cada una de
    -- las cuales esta formada por formulas de fs equivalentes entre si.
equivalentes :: [FProp] ->[[FProp]]