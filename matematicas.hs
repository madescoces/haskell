cuadrado :: Num a => a -> a
cuadrado x = x*x

polinomio :: Num a => a -> a
polinomio x = 2*x + cuadrado x