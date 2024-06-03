{-
    comentario
    multilinea
-}
miNumeroFavorito :: Integer
miNumeroFavorito = 3

miDecimal :: Double
miDecimal = 2.3

miInicial :: Char
miInicial = 'A'

tuNombre :: [Char] -- String
tuNombre = "Pablo"

tuOtroNombre :: [Char]
tuOtroNombre = ['P', 'e', 'd', 'r', 'o']

listaNumeros :: [Integer]
listaNumeros = [1, 2, 3, 4, 5, 6]

tuplaCoordenadas :: (Integer, Integer)
tuplaCoordenadas = (3, 2)

aniadirALista :: [a] -> a -> [a]
aniadirALista l e = e : l

quitarLista :: [a] -> a
quitarLista (e:l) = e

-- listado por intencion ( se limita a un filtro )
multiplosDeTres :: [Integer]
multiplosDeTres = [n | n <- [1 ..], mod n 3 == 0]

data Persona = UnaPersona
  { nombre     :: String
  , edad       :: Int
  , barrio     :: String
  , casa       :: Bool
  , lineaSubte :: Char
  } deriving (Show, Eq) -- para poder mostrar una persona y para poder compararlas.

juan :: Persona
juan = UnaPersona "Juan Perez" 20 "Boedo" True 'e'

maria :: Persona
maria = UnaPersona "Maria Gonzalez" 30 "Palermo" False 'd'

cacho :: Persona
cacho = UnaPersona "Cacho Castania" 60 "Retiro" True 'c'

saludar :: Persona -> String
saludar alguien = "Hola " ++ nombre alguien

irAComprar :: Persona -> String
irAComprar alguien@(UnaPersona _ _ "Boedo" _ _) =
  saludar alguien ++ " y no soy de San Lorenzo"
irAComprar alguien@(UnaPersona _ _ _ False _) = saludar alguien
irAComprar alguien = saludar alguien ++ " de " ++ barrio alguien

cambiarBarrio :: String -> Persona -> Persona
cambiarBarrio nuevoBarrio alguien = alguien {barrio = nuevoBarrio}

buscarLinea :: String -> Char
buscarLinea "Versalles" = 'a'
buscarLinea "Boedo"     = 'e'
buscarLinea "Almagro"   = 'a'
buscarLinea "Palermo"   = 'd'
buscarLinea _           = 'h'

cumplirVariosAnios :: Int -> Persona -> Persona
cumplirVariosAnios cantidad alguien = alguien {edad = edad alguien + cantidad}

cumplirAnios :: Persona -> Persona
cumplirAnios alguien = cumplirVariosAnios 1 alguien
