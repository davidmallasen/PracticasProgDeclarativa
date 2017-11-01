-- La poblacion de India a final de 2014 era de 1267 millones de habitantes, 
-- con un crecimiento del 0.78% respecto al a~no anterior. Los datos analogos 
-- para China fueron 1368 millones y 0.70 %, respectivamente. Suponiendo que 
-- los ritmos de crecimiento se mantienen constantes a lo largo de los anyos,
-- calcula mediante Haskell  lo siguiente:

-- Las poblaciones de ambos paises en 2013 y en 2105.
pobl n porc ini = poblAux n porc 2014 ini
    where poblAux n porc m pob
            | n == m    = pob
            | n < m     = poblAux n porc (m-1) (pob - pob*(porc/100))
            | n > m     = poblAux n porc (m+1) (pob + pob*(porc/100))
            --???????????????????????????????