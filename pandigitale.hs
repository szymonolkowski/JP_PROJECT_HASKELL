-- Funkcja obliczająca silnię
fact :: Integer -> Integer
fact n = product [1..n]

-- Wzór na permutacje z powtórzeniami: n! / (c1! * c2! * ...)
-- Oblicza na ile sposobów można ułożyć cyfry z podanej listy liczników
perms :: [Int] -> Integer
perms counts = fact (toInteger (sum counts)) `div` product (map (fact . toInteger) counts)

-- Oblicza permutacje, ale odejmuje te, które zaczynają się od zera
-- (Zero nie może być na najstarszej pozycji)
validPerms :: [Int] -> Integer
validPerms counts
    | counts !! 0 == 0 = total -- Jeśli nie ma zer, to wszystkie są ok
    | otherwise        = total - startsWithZero
  where
    total = perms counts
    -- Jeśli zabierzemy jedno zero na początek:
    countsWithoutZero = (head counts - 1) : tail counts
    startsWithZero = perms countsWithoutZero

-- Główna funkcja pomocnicza:
-- Generuje wszystkie końcówki (sufiksy) o długości k
-- Sprawdza czy końcówka spełnia warunek podzielności
-- Dla pasujących końcówek liczy kombinatorycznie resztę liczby
solveSuffix :: Int -> Int -> Integer
solveSuffix n k = sum [ calc restCounts | suffix <- generateSuffixes k
                                        , fromDigits suffix `mod` n == 0
                                        , let restCounts = subtractCounts initialCounts suffix
                                        , all (>=0) restCounts ]
  where
    initialCounts = replicate 10 2
    
    -- Funkcja generująca wszystkie listy cyfr o długości len
    generateSuffixes 0 = [[]]
    generateSuffixes len = [d:rest | d <- [0..9], rest <- generateSuffixes (len-1)]
    
    fromDigits = foldl (\acc d -> acc * 10 + d) 0
    
    subtractCounts counts [] = counts
    subtractCounts counts (x:xs) = 
        let c = counts !! x 
        in subtractCounts (take x counts ++ [c-1] ++ drop (x+1) counts) xs

    -- Dla reszty cyfr (prefiks) musimy pamiętać, że pierwsza cyfra (całej liczby) nie może być 0.
    -- Suffix jest na końcu, więc prefiks jest na początku liczby.
    calc cts = validPerms cts

-- Rozwiązanie kombinatoryczne dla 11 (suma poz. nieparzystych - suma poz. parzystych)
solve11 :: Integer
solve11 = sum [ waysOdd * waysEven
              | (oddSet, oddSum) <- subsets 10 initialCounts 0
              , (2 * oddSum - 90) `mod` 11 == 0 -- Warunek podzielności przez 11
              , let evenSet = zipWith (-) initialCounts oddSet
              , let waysOdd = validPerms oddSet -- Pozycje nieparzyste zawierają pozycję startową (1), więc dbamy o 0
              , let waysEven = perms evenSet    -- Pozycje parzyste są wewnątrz liczby, 0 dozwolone
              ]
  where
    initialCounts = replicate 10 2
    -- Generuje podzbiory o rozmiarze k: (użyte_liczniki, suma_cyfr)
    subsets 0 _ s = [(replicate 10 0, s)]
    subsets k (c:cs) currentSum = 
        [ (taken : restCounts, finalSum)
        | takeCnt <- [0 .. min k c] -- Ile wziąć tej cyfry (0, 1 lub 2)
        , let val =  (length initialCounts - length cs - 1)
        , (restCounts, finalSum) <- subsets (k - takeCnt) cs (currentSum + takeCnt * val)
        , let taken = takeCnt
        ]
    subsets _ [] _ = []

-- Rozwiązanie dla 7 (Algorytm iteracyjny/DP, bo brak prostej reguły sufiksowej)
-- Rozwiązanie dla 7 (Algorytm iteracyjny, sumowanie stanów w liście)
solve7 :: Integer
solve7 = sum [count | ((_, r), count) <- foldl step initial [1..20], r == 0]
  where
    -- Stan początkowy: [((UżyteCyfry, Reszta), Ilość)]
    initial = [((replicate 10 0, 0), 1)] 

    -- Krok algorytmu
    step states _ = compress [ ((addCnt mask d, (r * 10 + d) `mod` 7), count) -- TU BYŁ BŁĄD: Teraz jest para ((Klucz), Wartość)
                             | ((mask, r), count) <- states
                             , d <- [0..9]
                             , mask !! d < 2
                             , length states < 2 || (sum mask /= 0 || d /= 0) ] -- Zero check na początku

    -- Pomocnicza funkcja zwiększająca licznik użycia cyfry d
    addCnt mask d = take d mask ++ [mask !! d + 1] ++ drop (d+1) mask

    -- Funkcja kompresująca: bierze listę par i sumuje wartości dla tych samych kluczy
    compress :: [(([Int], Int), Integer)] -> [(([Int], Int), Integer)]
    compress raw = foldl (\acc (k,v) -> insertOrAdd k v acc) [] raw

    -- Wstawianie do listy (jeśli klucz istnieje, dodaj wartość, wpp. wstaw nowy)
    insertOrAdd k v [] = [(k,v)]
    insertOrAdd k v ((kx,vx):xs) 
        | k == kx   = (kx, vx + v) : xs 
        | otherwise = (kx,vx) : insertOrAdd k v xs
-- Główny dispatcher
solve :: Int -> Integer
solve n
  | n == 1 || n == 3 || n == 9 = validPerms (replicate 10 2) -- Zawsze podzielne
  | n == 2 || n == 5 || n == 10 = solveSuffix n 1            -- Ostatnia cyfra
  | n == 6                      = solveSuffix 2 1            -- To samo co div przez 2 (bo suma 90 dzieli sie przez 3)
  | n == 4                      = solveSuffix 4 2            -- Ostatnie 2 cyfry
  | n == 8                      = solveSuffix 8 3            -- Ostatnie 3 cyfry
  | n == 11                     = solve11                    -- Reguła sum nieparzystych/parzystych
  | n == 7                      = solve7                     -- Wyjątek (brak prostej reguły)
  | otherwise                   = 0

main :: IO ()
main = do
    print (solve 7)  -- Zmień n tutaj