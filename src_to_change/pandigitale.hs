fact :: Integer -> Integer
fact n = product [1..n]



perms :: [Int] -> Integer
perms counts = fact (toInteger (sum counts)) `div` product (map (fact . toInteger) counts)



validPerms :: [Int] -> Integer
validPerms counts
    | head counts == 0 = total -- Jeśli nie ma zer, to wszystkie są ok
    | otherwise        = total - startsWithZero
  where
    total = perms counts
    countsWithoutZero = (head counts - 1) : tail counts
    startsWithZero = perms countsWithoutZero



solveSuffix :: Int -> Int -> Integer
solveSuffix n k = sum [ calc restCounts | suffix <- generateSuffixes k
                                        , fromDigits suffix `mod` n == 0
                                        , let restCounts = subtractCounts initialCounts suffix
                                        , all (>=0) restCounts ]
  where
    initialCounts = replicate 10 2

    generateSuffixes 0 = [[]]
    generateSuffixes len = [d:rest | d <- [0..9], rest <- generateSuffixes (len-1)]

    fromDigits = foldl (\acc d -> acc * 10 + d) 0

    subtractCounts counts [] = counts
    subtractCounts counts (x:xs) =
        let c = counts !! x
        in subtractCounts (take x counts ++ [c-1] ++ drop (x+1) counts) xs

    calc = validPerms

solve11 :: Integer
solve11 = sum [ waysOdd * waysEven
            | (oddSet, oddSum) <- subsets 10 initialCounts 0
            , (2 * oddSum - 90) `mod` 11 == 0
            , let evenSet = zipWith (-) initialCounts oddSet
            , let waysOdd = validPerms oddSet
            , let waysEven = perms evenSet
            ]
    where
    initialCounts = replicate 10 2
    subsets 0 _ s = [(replicate 10 0, s)]
    subsets k (c:cs) currentSum =
        [ (taken : restCounts, finalSum)
        | takeCnt <- [0 .. min k c] -- Ile wziąć tej cyfry (0, 1 lub 2)
        , let val =  length initialCounts - length cs - 1
        , (restCounts, finalSum) <- subsets (k - takeCnt) cs (currentSum + takeCnt * val)
        , let taken = takeCnt
        ]
    subsets _ [] _ = []


solve :: Int -> Integer
solve n
  | n == 1 || n == 3 || n == 9 = validPerms (replicate 10 2) -- Zawsze podzielne
  | n == 2 || n == 5 || n == 10 = solveSuffix n 1            -- Ostatnia cyfra
  | n == 6                      = solveSuffix 2 1            -- To samo co div przez 2 (bo suma 90 dzieli sie przez 3)
  | n == 4                      = solveSuffix 4 2            -- Ostatnie 2 cyfry
  | n == 8                      = solveSuffix 8 3            -- Ostatnie 3 cyfry
  | n == 11                     = solve11                    -- Reguła sum nieparzystych/parzystych
  {- | n == 7                      = solve7                     -- Wyjątek (brak prostej reguły) -}
  | otherwise                   = 0

main :: IO ()
main = do
    print (solve 7)