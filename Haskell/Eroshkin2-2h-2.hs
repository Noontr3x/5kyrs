countTrue :: [Bool] -> Integer
countTrue bools = toInteger (length (filter (== True) bools))

main :: IO ()
main = do
    putStrLn "Введите список значений [True, False, ...], разделенных запятыми:"
    input <- getLine
    let boolList = read input :: [Bool]
    let result = countTrue boolList
    putStrLn $ "Количество значений True в списке: " ++ show result
