signo :: (Ord a, Num a) => a -> Char

signo numero
  | numero > 0 = '+' 
  | numero < 0 = '-'
  | otherwise = '0'