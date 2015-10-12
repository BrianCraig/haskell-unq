module Pokemon(Pokemon) where
import TP1

type Nombre = String
data Tipo = Agua | Fuego | Planta deriving (Show, Eq)
type Energia = Float

data Pokemon = P Nombre Tipo Energia deriving (Show)
type Entrenador = [Pokemon]

nuevoP :: Nombre -> Tipo -> Energia -> Pokemon
nuevoP n t e = P n t e

elementoGanador :: Tipo -> Tipo
elementoGanador Fuego = Agua
elementoGanador Planta = Fuego
elementoGanador Agua = Planta

tipo :: Pokemon -> Tipo
tipo (P _ t _) = t

tieneEnergia :: Pokemon -> Bool
tieneEnergia (P _ _ e) = e > 0

leGanaA :: Pokemon -> Pokemon -> Bool
leGanaA p1 p2 = (elementoGanador (tipo p2)) == (tipo p1)

capturarPokemon :: Pokemon -> Entrenador -> Entrenador
capturarPokemon p e = (p:e)

cantidadDePokemons :: Entrenador -> Int
cantidadDePokemons e = TP1.longitud e

pokemonsDeTipo :: Tipo -> Entrenador -> Entrenador
pokemonsDeTipo _ [] = []
pokemonsDeTipo t (p:ps) = if t == (tipo p)
                            then (p:(pokemonsDeTipo t ps))
                            else pokemonsDeTipo t ps

cantidadDePokemonsDeTipo :: Tipo -> Entrenador -> Int
cantidadDePokemonsDeTipo t e = cantidadDePokemons (pokemonsDeTipo t e)

lePuedeGanar :: Entrenador -> Pokemon -> Bool
lePuedeGanar [] _ = False
lePuedeGanar (ep:eps) p = TP1.orLogico (leGanaA ep p) (lePuedeGanar eps p)

tieneUnPokemonConEnergia :: Entrenador -> Bool
tieneUnPokemonConEnergia [] = False
tieneUnPokemonConEnergia (p:ps) = TP1.orLogico (tieneEnergia p) (tieneUnPokemonConEnergia ps)

puedePelear :: Tipo -> Entrenador -> Bool
puedePelear t e = tieneUnPokemonConEnergia (pokemonsDeTipo t e)

puedenPelear :: Tipo -> Entrenador -> Entrenador -> Bool
puedenPelear t e1 e2 = TP1.andLogico (puedePelear t e1) (puedePelear t e2)

esExperto :: Entrenador -> Bool
esExperto e = 
  let verificarTipos :: Entrenador -> Bool -> Bool -> Bool -> Bool
      verificarTipos [] ta tf tp = TP1.andLogico ta (TP1.andLogico tf tp)
      verificarTipos (p:ps) ta tf tp = verificarTipos ps (TP1.orLogico ta ((tipo p) == Agua)) (TP1.orLogico tf ((tipo p) == Fuego)) (TP1.orLogico tp ((tipo p) == Planta))
  in verificarTipos e False False False

fiestaPokemon :: [Entrenador] -> [Pokemon]
fiestaPokemon [] = []
fiestaPokemon (e:es) = e ++ fiestaPokemon es

-- datos Test

p1 = nuevoP "Chikorita" Fuego 0.9
p2 = nuevoP "Melvin de Chocokrispis" Agua 0.7
p3 = nuevoP "Gaseosin" Planta 0.3
p4 = nuevoP "Muertin" Fuego 0.0
p5 = nuevoP "Bulbasur" Fuego 0.4

