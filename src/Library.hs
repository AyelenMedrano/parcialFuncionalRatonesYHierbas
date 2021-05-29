module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Raton = UnRaton{
    nombre :: String,
    edad :: Number,
    peso :: Number,
    enfermedades :: [Enfermedad]
} deriving (Show, Eq)

type Enfermedad = String

-- 1)
cerebro = UnRaton "Cerebro" 9 0.2 ["brucelosis", "sarampion", "tuberculosis"]
bicenterrata = UnRaton "Bicenterrata" 256 0.2 []
huesudo = UnRaton "Huesudo" 4 10 ["alta obesidad", "sinusitis"]

--2)

type Hierba =  Raton -> Raton

hierbaBuena :: Hierba
hierbaBuena raton = raton {edad = sqrt (edad raton)}

hierbaVerde :: String -> Hierba
hierbaVerde forma raton = eliminarEnfermedadesSegun (noTerminaEn forma) raton

noTerminaEn :: String -> Enfermedad -> Bool
noTerminaEn forma enfermedad = (ultimosNElementos enfermedad forma) == forma

ultimosNElementos :: Enfermedad -> String -> String
ultimosNElementos enfermedad forma = reverse (take (length forma ) (reverse enfermedad))

eliminarEnfermedadesSegun ::(Enfermedad-> Bool) -> Raton -> Raton
eliminarEnfermedadesSegun criterio raton = raton { enfermedades = filter criterio (enfermedades raton)}

alcachofa :: Hierba
alcachofa raton | peso raton > 2 = perderPeso (peso raton * 0.1) raton
                | otherwise = perderPeso (peso raton * 0.05) raton

perderPeso :: Number -> Raton -> Raton
perderPeso cantidad raton = raton { peso = max 0 (peso raton - cantidad)}

hierbaZort :: Hierba
hierbaZort raton = raton { nombre = "Pinky", edad = 0, enfermedades = []}

hierbaDelDiablo :: Hierba
hierbaDelDiablo = eliminarEnfermedadesSegun ((<10).length) . perderPeso 0.1

--3)
pondsAntiAge :: [Hierba]
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]

reduceFatFast :: Number -> [Hierba]
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ take potencia (repeat alcachofa)

pdepCilina :: [Hierba]
pdepCilina = map hierbaVerde sufijosInfecciosas 

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

--4)

--a)
cantidadIdeal :: (Number -> Bool) -> Number
cantidadIdeal condicion =  head (filter condicion [1..]) 

--b)
lograEstabilizar :: [Hierba] -> [Raton] -> Bool
lograEstabilizar hierbas ratones = enfermedadesMenorA3 (ratonesYaAplicados hierbas ratones) && sinSobrepeso  (ratonesYaAplicados hierbas ratones)

ratonesYaAplicados :: [Hierba] -> [Raton] -> [Raton]
ratonesYaAplicados hierbas =  map (aplicarHierbasAUnRaton hierbas)

aplicarHierbasAUnRaton :: [Hierba] -> Raton -> Raton
aplicarHierbasAUnRaton hierbas raton = foldl aplicarUnaHierba raton hierbas

aplicarUnaHierba :: Raton -> Hierba -> Raton
aplicarUnaHierba raton unaHierba = unaHierba raton 

enfermedadesMenorA3 :: [Raton] -> Bool
enfermedadesMenorA3 = all ((<3) . length. enfermedades) 

sinSobrepeso :: [Raton] -> Bool
sinSobrepeso = all ((<=1) . peso) 

--c)
{-
reduceFatFast :: Number -> [Hierba]
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ take potencia (repeat alcachofa)

hierbaVerde :: String -> Hierba
hierbaVerde forma raton = eliminarEnfermedadesSegun (noTerminaEn forma) raton

alcachofa :: Hierba
alcachofa raton | peso raton > 2 = perderPeso (peso raton * 0.1) raton
                | otherwise = perderPeso (peso raton * 0.05) raton

experimentales = []

lograEstablizar (reduceFatFast potencia) ratones = True && True

minimo: va a eliminar todas las enfermedades que terminan en "obesidad"


-}

--5)
{-
a) Si todos los ratones quedan con menos de 1kg y sin enfermedades,
 entonces significa que el medicamento SI pudo estabilizar a la comunidad.
 Aunque si probamos un caso en el que si pase eso, pero con unalista infinita, 
 va a entrar en un ciclo infinito, ya que jamas va a dar False 

b) Si un ratón queda con 2kg y 4 enfermedades, este caso si es posible hacerlo 
 con una lista infinita, ya que cuando a penas sea falso va a dejar de seguir evaluando
 los otro ratones ya medicados. Ademas esto significa que el medicamento
 no estabilizo a TODOS los ratones
-}

{-
6)

a) No es necesario hacer ningun cambio, solo debo definir esa funcion como una hierba,
 y para construir un nuevo medicamento solo debo definir la lista. No, no es necesario
 modificar las funciones existentes

b) Imagino que lo pregunta por si es que construi las hierbas o los medicamentos como 
 un data en el caso de las hierbas, o como una tupla en el caso de los medicamentos.

c) No es necesario modificar ninguna funcion, si en los casos de hierbaDelDiablo y 
 sinSobrepeso nos dan el valor de la condicion en libras. En caso contrario en necesario 
 agregar una funcion que pase de Kg a Libras, y modificar las funciones antes mencionadas.
-}