sumaDeMultiplosDeDebajo :: [Int] -> Int -> Int
sumaDeMultiplosDeDebajo [] _ = 0
sumaDeMultiplosDeDebajo xs l = sum (multiplosDeEn xs [1..(l - 1)])

multiplosDeEn :: [Int] -> [Int] -> [Int]
multiplosDeEn _ [] = []
multiplosDeEn xs (y:ys) = if (esMultiploDe y xs)
  then y : (multiplosDeEn xs ys)
  else multiplosDeEn xs ys

esMultiploDe :: Int -> [Int] -> Bool
esMultiploDe n [] = False
esMultiploDe n (x : xs) = (mod n x) == 0 || (esMultiploDe n xs)


suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + (suma xs)
