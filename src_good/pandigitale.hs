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
      zmniejszLicznikZer []     = []

allPandigitals :: Integer
allPandigitals = validPerms (replicate 10 2)

zmniejszLicznik :: Int -> [Int] -> [Int]
zmniejszLicznik _ [] = []
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


-- !LOGIKA DLA N = 11
czySumaSpelniaWarunek :: Int -> Bool
czySumaSpelniaWarunek sumaNieparzystych =
    (2 * sumaNieparzystych - 90) `mod` 11 == 0

generujPodzialy :: Int -> [Int] -> Int -> [([Int], Int)]
generujPodzialy 0 _ suma = [(replicate 10 0, suma)]
generujPodzialy _ [] _   = []
generujPodzialy ileMiejsc (dostepnaIlosc:pozostaleLiczniki) obecnaSuma =
    [ (iloscDoWybrania : resztaDecyzji, sumaKoncowa)
    | iloscDoWybrania <- [0 .. min ileMiejsc dostepnaIlosc]
    , let wartoscCyfry = 10 - 1 - length pozostaleLiczniki
    , let nowaSuma = obecnaSuma + (iloscDoWybrania * wartoscCyfry)
    , (resztaDecyzji, sumaKoncowa) <- generujPodzialy (ileMiejsc - iloscDoWybrania) pozostaleLiczniki nowaSuma
    ]

solve11 :: Integer
solve11 = sum
    [ sposobyUlozenia
    | (zestawNieparzysty, suma) <- generujPodzialy 10 (replicate 10 2) 0
    , czySumaSpelniaWarunek suma
    , let zestawParzysty = zipWith (-) (replicate 10 2) zestawNieparzysty
    , let sposobyUlozenia = validPerms zestawNieparzysty * perms zestawParzysty
    ]


-- !LOGIKA DLA N = 7 (Programowanie Dynamiczne)
solve7 :: Integer
solve7 =
    let
        start :: [([Int], Int, Integer)]
        start = [(replicate 10 2, 0, 1)]

        wyniki :: [([Int], Int, Integer)]
        wyniki = foldl krokAlgorytmu start [1..20]
    in
        sum [count | (_, r, count) <- wyniki, r == 0]

krokAlgorytmu :: [([Int], Int, Integer)] -> Int -> [([Int], Int, Integer)]
krokAlgorytmu stany numerKroku =
    scalajDuplikaty (sortujStany stanyRozszerzone)
  where
    stanyRozszerzone :: [([Int], Int, Integer)]
    stanyRozszerzone =
        [ (noweLiczniki, nowaReszta, count)
        | (liczniki, staraReszta, count) <- stany
        , cyfra <- [0..9]
        , let iloscTejCyfry = liczniki !! cyfra
        , iloscTejCyfry > 0
        , not (numerKroku == 1 && cyfra == 0)
        , let noweLiczniki = zmniejszLicznik cyfra liczniki
        , let nowaReszta = (staraReszta * 10 + cyfra) `mod` 7
        ]

scalajDuplikaty :: [([Int], Int, Integer)] -> [([Int], Int, Integer)]
scalajDuplikaty [] = []
scalajDuplikaty [x] = [x]
scalajDuplikaty (x:y:reszta)
    | tenSamStan x y = scalajDuplikaty (polacz x y : reszta)
    | otherwise      = x : scalajDuplikaty (y:reszta)
  where
    tenSamStan :: ([Int], Int, Integer) -> ([Int], Int, Integer) -> Bool
    tenSamStan (l1, r1, _) (l2, r2, _) = l1 == l2 && r1 == r2

    polacz :: ([Int], Int, Integer) -> ([Int], Int, Integer) -> ([Int], Int, Integer)
    polacz (l, r, c1) (_, _, c2) = (l, r, c1 + c2)

sortujStany :: [([Int], Int, Integer)] -> [([Int], Int, Integer)]
sortujStany [] = []
sortujStany [x] = [x]
sortujStany xs =
    let (lewa, prawa) = splitAt (length xs `div` 2) xs
    in scalListy (sortujStany lewa) (sortujStany prawa)

scalListy :: [([Int], Int, Integer)] -> [([Int], Int, Integer)] -> [([Int], Int, Integer)]
scalListy [] ys = ys
scalListy xs [] = xs
scalListy ((l1,r1,c1):xs) ((l2,r2,c2):ys)
    | (l1, r1) <= (l2, r2) = (l1,r1,c1) : scalListy xs ((l2,r2,c2):ys)
    | otherwise            = (l2,r2,c2) : scalListy ((l1,r1,c1):xs) ys



solve :: Integer -> Integer
solve n
  | n `elem` [1, 3, 9]  = allPandigitals
  | n `elem` [2, 5, 10] = solveSuffix n 1
  | n == 6              = solveSuffix 2 1
  | n == 4              = solveSuffix 4 2
  | n == 8              = solveSuffix 8 3
  | n == 11             = solve11
  | n == 7              = solve7
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
    putStrLn "Podzielne przez 7: "
    print (solve 7)
    putStrLn "Podzielne przez 8: "
    print (solve 8)
    putStrLn "Podzielne przez 10: "
    print (solve 10)
    putStrLn "Podzielne przez 11: "
    print (solve 11)