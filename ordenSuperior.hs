-- demostraciones de inferencias
espejo :: t -> t
espejo x = x

alista :: t -> [t]
alista x = [x]

atupla :: t -> u -> (t, u)
atupla x y = (x, y)

guarda :: Ord a => a -> a -> a
guarda x y
  | x > y = x
  | otherwise = y

-- orden superior
data Equipo = Equipo
  { nombre    :: String
  , jugadores :: [Jugador]
  , puntos    :: Int
  , dt        :: String
  } deriving (Show)

type Jugador = String

boca :: Equipo
boca = Equipo "Boca Juniors" ["Palermo", "Riquelme"] 10 "Bianchi"

river :: Equipo
river = Equipo "River Plate" ["Quinteros", "Martinez", "Prato"] 10 "Gallardo"

racing :: Equipo
racing = Equipo "Racing" [] 7 "Merlo"

equiposEjemplo :: [Equipo]
equiposEjemplo = [boca, river, racing]

ganaPartido :: Equipo -> Equipo
ganaPartido equipo = equipo {puntos = puntos equipo + 3}

empataPartido :: Equipo -> Equipo
empataPartido equipo = equipo {puntos = puntos equipo + 1}

pierdePartido :: Equipo -> Equipo
pierdePartido equipo = equipo

contratarJugador :: Jugador -> Equipo -> Equipo
contratarJugador jugador equipo =
  equipo {jugadores = jugador : jugadores equipo}

cotizacionEquipo :: Equipo -> Int
cotizacionEquipo equipo = puntos equipo + length (jugadores equipo)

equipoProfesional :: Equipo -> Bool
equipoProfesional equipo = cotizacionEquipo equipo > 8

equipoConEstrella :: Equipo -> Bool
equipoConEstrella equipo = elem "Riquelme" (jugadores equipo)

{- Notar como se repite la lógica -}
equiposBuenos :: Num a => [Equipo] -> a
equiposBuenos [] = 0
equiposBuenos (equipo:equiposRestantes)
  | equipoProfesional equipo = 1 + equiposBuenos equiposRestantes
  | otherwise = equiposBuenos equiposRestantes

equiposConEstrella :: Num a => [Equipo] -> a
equiposConEstrella [] = 0
equiposConEstrella (equipo:equiposRestantes)
  | equipoConEstrella equipo = 1 + equiposConEstrella equiposRestantes
  | otherwise = equiposConEstrella equiposRestantes

equiposCon :: (a -> Bool) -> [a] -> Int
equiposCon _ [] = 0
equiposCon otraFuncion (equipo:equiposRestantes)
  | otraFuncion equipo = 1 + equiposCon otraFuncion equiposRestantes
  | otherwise = equiposCon otraFuncion equiposRestantes
  
{- orden superior propias de haskell
  filter even [1,2,3,4,5,6,7,8]
  map sqrt [1,2,3,4,5,6,7,8]
  sum [1,2,3,4,5,6,7,8]
  concatMap [lista] [lista] <-- flatmap
  all bool []
  any bool []

  lambda
  map (\ e -> contratarJugador "Diego" e)
  map (\ e -> e*2 ) [1,2,3,4,5]
-}
