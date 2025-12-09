-- Funkcja obliczeniowa (Twoja oryginalna funkcja)
findPytriples :: Int -> [(Int, Int, Int)]
findPytriples n =
    [ (a, b, c)
    | c <- reverse [1 .. n `div` 2 - 1] -- Uproszczony zakres dla c
    , let a_plus_b = n - c
    , let min_b = a_plus_b `div` 2
    , b <- reverse [1 .. c - 1]
    , b >= min_b -- Dodano warunek b >= a (czyli b >= (a_plus_b - b))
    , let a = a_plus_b - b
    , gcd a (gcd b c) == 1
    , a > 0
    -- Warunek Pitagorejski: a^2 + b^2 = c^2
    , a * a + b * b == c * c
    ]

-- Główna funkcja IO (punkt wejścia programu)
main :: IO ()
main = do
    putStrLn "Podaj sumę boków N (np. 12):"
    inputStr <- getLine
    
    -- Konwersja wejścia na liczbę całkowitą Int
    case reads inputStr :: [(Int, String)] of
        [(n, "")] | n >= 12 -> do
            -- Wymaga N >= 12, aby móc utworzyć trójkąt 3, 4, 5
            putStrLn $ "Szukam trójek pitagorejskich dla sumy N = " ++ show n ++ ":"
            let results = findPytriples n
            
            if null results
                then putStrLn "Nie znaleziono trójek pitagorejskich dla tej sumy."
                else mapM_ print results
            
        _ -> putStrLn "Niepoprawne wejście. Podaj poprawną liczbę całkowitą (N >= 12)."