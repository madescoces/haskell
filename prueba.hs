largoDelListon :: Int
largoDelListon = 4

meAlcanza :: Float -> Bool
meAlcanza largo = largo <= fromIntegral (largoDelListon)

cuantoMeSobra :: Float -> Float
cuantoMeSobra cantidad = fromIntegral (largoDelListon) - cantidad

tuplita :: (Int, Char, String, Bool)
tuplita = (1, 'c', "asdf", False)

type Valor = Int

tuplita2 :: (Valor, Char, String, Bool)
tuplita2 = (1, 'c', "asdf", False)

data TuplitaData = UnaTuplitaData
  { valor   :: Int
  , inicial :: Char
  , texto   :: String
  , valido  :: Bool
  }

nuevaTuplita :: TuplitaData
nuevaTuplita = UnaTuplitaData 1 'c' "asdf" True

otraTuplita :: TuplitaData
otraTuplita = nuevaTuplita {valor = 3}