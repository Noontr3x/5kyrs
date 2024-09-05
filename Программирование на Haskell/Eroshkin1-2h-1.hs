import Data.List (sort)

isRectangular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (x1, y1) (x2, y2) (x3, y3) = 
    let

        side1 = (x2 - x1)^2 + (y2 - y1)^2
        side2 = (x3 - x2)^2 + (y3 - y2)^2
        side3 = (x1 - x3)^2 + (y1 - y3)^2

        [a, b, c] = sort [side1, side2, side3]
    in

        a + b == c

main :: IO ()
main = do
    putStrLn "Введите координаты первой точки (x1 y1):"
    input1 <- getLine
    let [x1, y1] = map read (words input1) :: [Double]

    putStrLn "Введите координаты второй точки (x2 y2):"
    input2 <- getLine
    let [x2, y2] = map read (words input2) :: [Double]

    putStrLn "Введите координаты третьей точки (x3 y3):"
    input3 <- getLine
    let [x3, y3] = map read (words input3) :: [Double]
    
    if isRectangular (x1, y1) (x2, y2) (x3, y3)
    then putStrLn "Треугольник прямоугольный."
    else putStrLn "Треугольник не является прямоугольным."
