-- !permutacje itd.
fact :: Int -> Integer
fact n = product [1 .. toInteger n]

perms :: [Int] -> Integer
perms counts = fact (sum counts) `div` product (map fact counts)

validPerms :: [Int] -> Integer
validPerms counts = total - startsWithZero
    where
      total :: Integer
      total = perms counts

      startsWithZero :: Integer
      startsWithZero = perms (zmniejszLicznikZer counts)

      zmniejszLicznikZer :: [Int] -> [Int]
      zmniejszLicznikZer (z:zs) = (z-1) : zs

allPandigitals :: Integer
allPandigitals = validPerms (replicate 10 2)

zmniejszLicznik :: Int -> [Int] -> [Int]
zmniejszLicznik 0 (x:xs) = (x - 1) : xs
zmniejszLicznik n (x:xs) = x : zmniejszLicznik (n - 1) xs


-- !LOGIKA SUFIKSOWA (N = 2, 4, 5, 6, 8, 10)
solveSuffix :: Integer -> Int -> Integer
solveSuffix n k = sum
    [ validPerms resztaLicznikow
    | sufiks <- generujSufiksy k
    , toNum sufiks `mod` n == 0
    , let resztaLicznikow = odejmijSufiks (replicate 10 2) sufiks
    , all (>=0) resztaLicznikow
    ]
    where
      generujSufiksy :: Int -> [[Int]]
      generujSufiksy 0 = [[]]
      generujSufiksy len =
        [d:reszta | d <- [0..9], reszta <- generujSufiksy (len-1)] 

      toNum :: [Int] -> Integer
      toNum lista = licz 0 lista
        where
          licz :: Integer -> [Int] -> Integer
          licz wynik [] = wynik
          licz wynik (cyfra:reszta) = licz (wynik * 10 + toInteger cyfra) reszta

      odejmijSufiks :: [Int] -> [Int] -> [Int]
      odejmijSufiks counts [] = counts
      odejmijSufiks counts (x:xs) =
          let nowaPula = zmniejszLicznik x counts
          in odejmijSufiks nowaPula xs



solve :: Integer -> Integer
solve n
  | n `elem` [1, 3, 9]  = allPandigitals
  | n `elem` [2, 5, 10] = solveSuffix n 1
  | n == 6              = solveSuffix 2 1
  | n == 4              = solveSuffix 4 2
  | n == 8              = solveSuffix 8 3
  | otherwise           = 0


main :: IO ()
main = do
    putStrLn "Podzielne przez 1/3/9: "
    print (solve 1)
    putStrLn "Podzielne przez 2: "
    print (solve 2)
    putStrLn "Podzielne przez 4: "
    print (solve 4)
    putStrLn "Podzielne przez 5: "
    print (solve 5)
    putStrLn "Podzielne przez 6: "
    print (solve 6)
    putStrLn "Podzielne przez 8: "
    print (solve 8)
    putStrLn "Podzielne przez 10: "
    print (solve 10)
