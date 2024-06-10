{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use null" #-}

data Libro = Libro {
  autor:: String,
  obra:: String
} deriving (Show, Eq)

libros :: [Libro]
libros = [
  Libro "elsaBornemann" "socorro",
  Libro "neilGaiman" "sandman",
  Libro "alanMoore" "watchmen",
  Libro "neilGaiman" "americanGods",
  Libro "neilGaiman" "buenosPresagios",
  Libro "terryPratchett" "buenosPresagios",
  Libro "brianAzarello" "cienBalas",
  Libro "warenElis" "planetary",
  Libro "frankMiller" "elCaballeroOscuroRegresa",
  Libro "stephenKing" "elCaballeroOscuroRegresa",
  Libro "frankMiller" "batmanAnioUno",
  Libro "isaacAsimov" "fundacion",
  Libro "isaacAsimov" "yoRobot",
  Libro "isaacAsimov" "elFinDeLaEternidad",
  Libro "isaacAsimov" "laBusquedaDeLosElementos",
  Libro "joseHernandez" "martinFierro",
  Libro "stephenKing" "it",
  Libro "stephenKing" "misery",
  Libro "stephenKing" "carrie",
  Libro "stephenKing" "elJuegoDeGerald",
  Libro "julioCortazar" "rayuela",
  Libro "jorgeLuisBorges" "ficciones",
  Libro "jorgeLuisBorges" "elAleph",
  Libro "horacioQuiroga" "cuentosDeLaSelva",
  Libro "horacioQuiroga" "cuentosDeLocuraAmorYMuerte"
  ]

-- condicionEnLista condicion lista parametro = condicion (\libro -> obra libro == parametro) lista

-- ¿Es cierto que alguien escribió una determinada obra?
escribio:: String -> Bool
-- escribio obra = any (tituloCoincide obra) libros
escribio obraConsultar = any (\libro -> obra libro == obraConsultar) libros
-- escribio obraConsultar = condicionEnLista any libros obraConsultar

tituloCoincide :: String -> Libro -> Bool
tituloCoincide obraConsultar libro = obraConsultar == obra libro

-- ¿Quién o quienes escribieron una obra?
quienEscribio :: String -> [String]
quienEscribio obraConsultar = map autor (filter (\libro -> obra libro == obraConsultar) libros)

-- ¿Qué obra/s escribió cierta persona?
queEscribio :: String -> [String]
queEscribio autorConsultar = map obra (filter (\libro -> autor libro == autorConsultar) libros)

-- Si es cierto que cierta persona escribió alguna obra, sin importar cual.
esEscritor :: String -> Bool
esEscritor autorConsultar = any (\libro -> autor libro == autorConsultar) libros

-- Si es cierto que cierta obra existe.
existe :: String -> Bool
existe obraConsultar = any (\libro -> obra libro == obraConsultar) libros

-- //// FORMA DISTINTA DE HACERLO ;) ///////////////////////////////////////////////////

type Obra = String
type Nombre = String

data Autor = Autor {
  nombre:: Nombre,
  obras:: [Obra]
} deriving (Show, Eq)

autores:: [Autor]
autores = [
  Autor "elsaBornemann" ["socorro"],
  Autor "neilGaiman" ["sandman"],
  Autor "alanMoore" ["watchmen"],
  Autor "neilGaiman" ["americanGods","buenosPresagios"],
  Autor "terryPratchett" ["buenosPresagios"],
  Autor "brianAzarello" ["cienBalas"],
  Autor "warenElis" ["planetary"],
  Autor "frankMiller" ["elCaballeroOscuroRegresa", "batmanAnioUno"],
  Autor "isaacAsimov" ["fundacion", "yoRobot", "elFinDeLaEternidad", "laBusquedaDeLosElementos"],
  Autor "joseHernandez" ["martinFierro"],
  Autor "stephenKing" ["it", "misery", "carrie", "elJuegoDeGerald"],
  Autor "julioCortazar" ["rayuela"],
  Autor "jorgeLuisBorges" ["ficciones", "elAleph"],
  Autor "horacioQuiroga" ["cuentosDeLaSelva", "cuentosDeLocuraAmorYMuerte"] 
  ]

-- Quien o quienes escribieron determinada obra?
quienesEscribieron :: Obra -> [Nombre]
quienesEscribieron obraBuscada = map nombre (filter (escribioObra obraBuscada) autores)

escribioObra :: Obra -> Autor -> Bool
escribioObra obraBuscada autor = obraBuscada `elem` obras autor

-- Es cierto que alguien escribió determinada obra?
alguienEscribio :: Obra -> Bool
alguienEscribio obraBuscada = length (quienesEscribieron obraBuscada) > 0

-- Qué obras escribió determinado autor?
buscarPorNombre :: Nombre -> [Autor] -> Maybe Autor
buscarPorNombre _ [] = Nothing
buscarPorNombre nombreBuscado (x:xs)
    | nombre x == nombreBuscado = Just x
    | otherwise = buscarPorNombre nombreBuscado xs

obrasDeAutor :: Nombre -> [Obra]
obrasDeAutor autorBuscado = 
  case buscarPorNombre autorBuscado autores of
    Just autor -> obras autor
    Nothing -> error "No existe dicho Autor"

-- Si es cierto que cierta persona escribó alguna obra, sin importar cual 
escribioUnaObra :: Nombre -> Bool
escribioUnaObra autorBuscado = any (\autor -> nombre autor == autorBuscado && length (obras autor) > 0 ) autores

-- Si es cierto que cierta obra existe
existeObra :: Obra -> Bool
existeObra obraBuscada = any (\autor -> obraBuscada `elem` obras autor) autores
