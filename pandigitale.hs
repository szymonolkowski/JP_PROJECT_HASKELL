import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Int (Int64)
import System.IO (hFlush, stdout)

-- Stałe pomocnicze
numDigits :: Int
numDigits = 10

maxMask :: Int
maxMask = (3 ^ numDigits) - 1  -- 59048 (2222222222 w systemie trójkowym)

-- Prekompilowane potęgi 3: [1, 3, 9, 27, ..., 19683]
powersOf3 :: [Int]
powersOf3 = map (3^) [0..9]

solve :: Int -> Int64
solve n = runST $ do
    let size = 3^numDigits * n
    dp <- newArray (0, size - 1) 0 :: ST s (STUArray s Int Int64)

    forM_ [1..9] $ \d -> do
        let mask = powersOf3 !! d
        let rem  = d `mod` n
        let idx  = mask * n + rem
        writeArray dp idx 1

    forM_ [0 .. maxMask - 1] $ \mask -> do
        forM_ [0 .. n - 1] $ \rem -> do
            let currentIdx = mask * n + rem
            count <- readArray dp currentIdx
            
            when (count > 0) $ do
                forM_ [0..9] $ \digit -> do
                    let p3 = powersOf3 !! digit
                    let usageCount = (mask `div` p3) `mod` 3
                    
                    when (usageCount < 2) $ do
                        let newMask = mask + p3
                        let newRem  = (rem * 10 + digit) `mod` n
                        let newIdx  = newMask * n + newRem
                        
                        prevVal <- readArray dp newIdx
                        writeArray dp newIdx (prevVal + count)

    readArray dp (maxMask * n + 0)11

main :: IO ()
main = do
    putStr "Podaj n (1 <= n <= 11): "
    hFlush stdout 
    input <- getLine
    let n = read input :: Int
    
    if n < 1 || n > 11
        then putStrLn "n musi być w zakresie 1-11."
        else do
            let result = solve n
            putStrLn $ "Liczba podwójnych pandigitali podzielnych przez " ++ show n ++ ":"
            print result