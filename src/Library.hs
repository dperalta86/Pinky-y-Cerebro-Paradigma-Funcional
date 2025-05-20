module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

{-
        === PRACTICA PARCIAL ===
            PINKY Y CEREBRO

-}

-- Punto 1

data Especies = PERRO | GATO | ELEFANTE | RATON deriving(Show, Eq)
data Animal = UnAnimal {
    coefIntelectualidad :: Number,
    especie :: Especies,
    capacidades :: [String]
}deriving(Show, Eq)

-- Definir animales de ejemplo
animalA = UnAnimal{
    coefIntelectualidad = 30,
    especie = PERRO,
    capacidades = ["ladrar", "morder", "auyar"]
}

animalB = UnAnimal{
    coefIntelectualidad = 10,
    especie = GATO,
    capacidades = ["mauyar", "morder", "araniar"]
}

elefanteA = UnAnimal{
    coefIntelectualidad = 10,
    especie = ELEFANTE,
    capacidades = ["pisada", "gritar"]
}

ratonA :: Animal
ratonA = UnAnimal{
    coefIntelectualidad = 10,
    especie = RATON,
    capacidades = ["correr", "esconderse"]
}

-- Para probar noTanCuerdo
ratonB :: Animal
ratonB = UnAnimal{
    coefIntelectualidad = 110,
    especie = RATON,
    capacidades = ["correr", "esconderse", "hacer narf", "hacer jasja", "hacer yaya"]
}
-- Para probar punto 4
raton4 :: Animal
raton4 = UnAnimal{
    coefIntelectualidad = 17,
    especie = RATON,
    capacidades = ["destruenglonir el mundo", "hacer planes desalmados"]
}

-- Punto B  

modificarCoefIntelectual :: Animal -> Number -> Animal
modificarCoefIntelectual animal nuevoCoeficiente = animal {coefIntelectualidad = nuevoCoeficiente}

inteligenciaSuperior :: Animal -> Number -> Animal
inteligenciaSuperior animal cantidad = modificarCoefIntelectual animal (coefIntelectualidad animal + cantidad)

pinkificar :: Animal -> Animal
pinkificar animal = animal {capacidades = []}

superPoderes :: Animal -> Animal
superPoderes animal
    | esElefante animal = animal {capacidades = "no tenerle miedo a los ratones" : capacidades animal}
    | esRaton animal && esIntelectual = animal {capacidades = "hablar" : capacidades animal}
    | otherwise = animal
    where
        esElefante = (==ELEFANTE). especie
        esRaton = (==RATON) . especie
        esIntelectual = tieneCoeficienteMayor animal 100

tieneCoeficienteMayor :: Animal -> Number -> Bool
tieneCoeficienteMayor animal coeficiente = coefIntelectualidad animal > coeficiente

-- Punto 3
esAntropomorfico :: Animal -> Bool
esAntropomorfico animal = puedeHablar animal && tieneCoeficienteMayor animal 60

puedeHablar :: Animal -> Bool
puedeHablar animal = "hablar" `elem` capacidades animal

noTanCuerdo :: Animal -> Bool
noTanCuerdo = (>2) . length . filter pinkiesco . capacidades

pinkiesco :: String -> Bool
pinkiesco habilidad = empiezaConHacer habilidad && palabraPinkiesca habilidad

empiezaConHacer :: String -> Bool
empiezaConHacer habilidad = take 6 habilidad == "hacer "

palabraPinkiesca :: String -> Bool
palabraPinkiesca habilidad = tieneMenosDeCincoLetras habilidad && tieneAlMenosUnaVocal habilidad

tieneMenosDeCincoLetras :: String -> Bool
tieneMenosDeCincoLetras = (<5) . length . ultimasLetras

ultimasLetras :: [a] -> [a]
ultimasLetras = drop 6

tieneAlMenosUnaVocal :: String -> Bool
tieneAlMenosUnaVocal habilidad = any esVocal (ultimasLetras habilidad)

esVocal :: Char -> Bool
esVocal caracter
    | caracter == 'a' = True
    | caracter == 'e' = True
    | caracter == 'i' = True
    | caracter == 'o' = True
    | caracter == 'u' = True
    | otherwise = False

-- Punto 4
data Transformaciones = INTELIGENCIA_SUPERIOR | PINKIFICAR | SUPERPODERES deriving (Show, Eq)
data CriterioExito = NO_TAN_CUERDO | ANTROPOMORFICO deriving (Show, Eq)

data Experimento = UnExperimento{
    nombreExperimento :: String,
    listaTransformaciones :: [Transformaciones],
    criterioExito :: CriterioExito
}deriving(Show, Eq)

experimentoA :: Experimento
experimentoA = UnExperimento {
    nombreExperimento = "Primer experimento",
    listaTransformaciones = [PINKIFICAR, SUPERPODERES],
    criterioExito = ANTROPOMORFICO
}

superExperimento :: Experimento
superExperimento = UnExperimento {
    nombreExperimento = "Super experimento",
    listaTransformaciones = [PINKIFICAR, INTELIGENCIA_SUPERIOR, SUPERPODERES],
    criterioExito = ANTROPOMORFICO
}

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experimento animal
    | criterioAntropomorfico = esAntropomorfico (hacerExperimento (listaTransformaciones experimento) animal)
    | criterioNoTanCuerdo = noTanCuerdo (hacerExperimento (listaTransformaciones experimento) animal)
    | otherwise = False
    where
        criterioAntropomorfico = (== ANTROPOMORFICO) (criterioExito experimento)
        criterioNoTanCuerdo = (== NO_TAN_CUERDO) (criterioExito experimento)

hacerExperimento :: [Transformaciones] -> Animal -> Animal
hacerExperimento listaTransformaciones animal = foldl agregarTransformacion animal listaTransformaciones

agregarTransformacion :: Animal -> Transformaciones -> Animal
agregarTransformacion animal transformacion
    | transformacion == INTELIGENCIA_SUPERIOR = inteligenciaSuperior animal 10
    | transformacion == PINKIFICAR = pinkificar animal
    | transformacion == SUPERPODERES = superPoderes animal
    | otherwise = animal

-- Punto 6
-- Solamente se podrían hacer experiemtos que tengan a INTELIGENCIA_SUPERIOR como transformación
-- ya que si no, el programa intentaria recorrer la lista de capacdades (lista infinita), finalizando
-- cuando ya no existan recursos. 
-- Ejemplo:
experimentoInteligenciaSuperior :: Experimento
experimentoInteligenciaSuperior = UnExperimento {
    nombreExperimento = "Experimento de inteligencia superior",
    listaTransformaciones = [INTELIGENCIA_SUPERIOR, INTELIGENCIA_SUPERIOR],
    criterioExito = NO_TAN_CUERDO
}