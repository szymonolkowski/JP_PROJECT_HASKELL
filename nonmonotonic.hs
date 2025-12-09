import Control.Monad (when)

-- Zoptymalizowana wersja bez użycia show/read (szybsza)
isNonMonotonic :: Int -> Bool
isNonMonotonic x 
    | x < 100 = False -- Liczby < 100 są zawsze monotoniczne
    | otherwise = 
        let digits = toDigits x
        in check digits False False
  where
    -- Zamienia liczbę na listę cyfr [1, 2, 3]
    toDigits :: Int -> [Int]
    toDigits 0 = []
    toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

    check :: [Int] -> Bool -> Bool -> Bool
    check (a:b:rest) inc dec =
        let inc' = inc || b > a
            dec' = dec || b < a
        in if inc' && dec' 
           then True -- Jest "bouncy" (niemonotoniczna)
           else check (b:rest) inc' dec'
    check _ _ _ = False

loop :: Double -> Int -> Int -> IO ()
loop targetP n countBouncy = do
    -- 1. Najpierw sprawdzamy aktualną liczbę n
    let isBouncy = isNonMonotonic n
    let newCount = if isBouncy then countBouncy + 1 else countBouncy
    
    -- 2. Obliczamy aktualną proporcję DLA n
    let currentP = fromIntegral newCount / fromIntegral n
    
    -- 3. Sprawdzamy warunek (Project Euler zazwyczaj szuka ==, tutaj szukamy > lub >=)
    -- Używamy >=, aby złapać moment dokładnego osiągnięcia progu
    if currentP >= targetP
        then do
            putStrLn "Znaleziono!"
            putStrLn $ "n = " ++ show n
            putStrLn $ "Proporcja = " ++ show currentP
            putStrLn $ "Liczba niemotonicznych = " ++ show newCount
        else 
            -- Rekurencja dla n + 1
            loop targetP (n + 1) newCount

main :: IO ()
main = do
    let p = 0.1 -- Zmieniłem na 50% dla testu (wynik powinien być 538)
    let startN = 100 -- Zaczynamy sprawdzanie od 100
    let startCount = 0 -- Poniżej 100 nie ma liczb niemotonicznych
    
    -- Uwaga: zaczynamy pętlę dla startN, a nie (startN + 1)
    loop p startN startCount