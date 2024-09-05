factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorialList :: Integer -> [Integer]
factorialList n = [factorial x | x <- [1..n]]

main :: IO ()
main = do
    putStrLn "Введите целое число n:"
    input <- getLine
    let n = read input :: Integer
    if n < 0 
    then putStrLn "Ошибка: число должно быть неотрицательным."
    else do
        let result = factorialList n
        putStrLn $ "Список факториалов до " ++ show n ++ ": " ++ show result
