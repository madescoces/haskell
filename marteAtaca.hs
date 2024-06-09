type MedidaContraEnfermedad = String

type Continente = String

type Habitantes = Int

data Enfermedad = UnaEnfermedad
  { tasaMortalidad :: Float
  , medida         :: MedidaContraEnfermedad
  } deriving (Show)

data Planeta = Planeta
  { poblacionTotal :: Habitantes
  , continetes     :: [Continente]
  , medidasTomadas :: [MedidaContraEnfermedad]
  } deriving (Show)

coronaVirus :: Enfermedad
coronaVirus = UnaEnfermedad 3.5 "Cuarentena"

dengue :: Enfermedad
dengue = UnaEnfermedad 2.7 "Evitar Acumulacion Agua"

pesteAmarrilla :: Enfermedad
pesteAmarrilla = UnaEnfermedad 1 "Lavarse Manos"

tierra :: Planeta
tierra =
  Planeta
    5004000495
    ["Asia", "America", "Europa", "Antartico", "Oceania"]
    ["Cuarentena"]

marte :: Planeta
marte = Planeta 2 [] ["Cuarententa", "Evitar Acumulacion Agua", "Lavarse Manos"]

venus :: Planeta
venus = Planeta 15 ["Comuna1", "Comuna2", "Comuna3"] ["Evitar Acumulacion Agua"]

pluto :: Planeta
pluto = Planeta 1000 [] []

combinarEnfermedad :: Enfermedad -> Enfermedad -> Enfermedad
combinarEnfermedad primeraEnfermedad segundaEnfermedad =
  UnaEnfermedad
    (tasaMortalidad primeraEnfermedad + tasaMortalidad segundaEnfermedad)
    (medida primeraEnfermedad)

tomarMedidas :: Planeta -> Enfermedad -> Planeta
tomarMedidas planeta enfermedad
  | planetaProtegidoContra planeta enfermedad = planeta
  | otherwise =
    planeta {medidasTomadas = medida enfermedad : medidasTomadas planeta}

planetaProtegidoContra :: Planeta -> Enfermedad -> Bool
planetaProtegidoContra planeta enfermedad =
  elem (medida enfermedad) (medidasTomadas planeta)

implementarMedida :: Planeta -> MedidaContraEnfermedad -> Planeta
implementarMedida planeta medida =
  planeta {medidasTomadas = medida : medidasTomadas planeta}

estimarDanioEnferemedad :: Planeta -> Enfermedad -> Int
estimarDanioEnferemedad planeta enfermedad =
  ceiling
    (tasaMortalidad enfermedad * fromIntegral (poblacionTotal planeta) / 100)

planetaAlHorno :: Planeta -> Enfermedad -> Bool
planetaAlHorno planeta enfermedad =
  estimarDanioEnferemedad planeta enfermedad >= 1000000

habitantesPorContinente :: Planeta -> Habitantes
habitantesPorContinente (Planeta pob [] _)   = pob
habitantesPorContinente (Planeta pob cont _) = pob `div` length cont

cantidadMedidas :: Planeta -> Int
cantidadMedidas planeta = length (medidasTomadas planeta)

demasiadasMedidas :: Planeta -> Bool
demasiadasMedidas planeta =
  cantidadMedidas planeta > habitantesPorContinente planeta

habitantesLuegoEnfermedad :: Planeta -> Enfermedad -> Int
habitantesLuegoEnfermedad planeta enfermedad =
  poblacionTotal planeta - estimarDanioEnferemedad planeta enfermedad

enfermedadAtacaPlaneta :: Planeta -> Enfermedad -> Planeta
enfermedadAtacaPlaneta planeta enfermedad
  | planetaProtegidoContra planeta enfermedad = planeta
  | otherwise =
    tomarMedidas
      planeta {poblacionTotal = habitantesLuegoEnfermedad planeta enfermedad}
      enfermedad

enfermedadMutadaA :: Enfermedad
enfermedadMutadaA = combinarEnfermedad coronaVirus dengue

enfermedadMutadaB :: Enfermedad
enfermedadMutadaB = combinarEnfermedad dengue coronaVirus

enfermedadMasDanio :: Enfermedad -> Enfermedad -> Planeta -> Enfermedad
enfermedadMasDanio enfermedadA enfermedadB planeta
  | estimarDanioEnferemedad planeta enfermedadA
      > estimarDanioEnferemedad planeta enfermedadB = enfermedadA
  | otherwise = enfermedadB