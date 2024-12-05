
data Font = Courier | Lucida | Fixedsys deriving (Show, Eq)

data Figure = Circle (Float, Float) Float             
            | Rectangle (Float, Float) (Float, Float)  
            | Triangle (Float, Float) (Float, Float) (Float, Float)  
            | TextBox (Float, Float) Font String       
            deriving (Show)

area :: Figure -> Float
area (Circle _ r) = pi * r * r
area (Rectangle (x1, y1) (x2, y2)) = abs ((x2 - x1) * (y2 - y1))
area (Triangle (x1, y1) (x2, y2) (x3, y3)) = abs ((x1*(y2 - y3) + x2*(y3 - y1) + x3*(y1 - y2)) / 2)
area (TextBox _ font text) = let (width, height) = fontSize font in fromIntegral (length text) * width * height

fontSize :: Font -> (Float, Float)
fontSize Courier = (10, 20)  
fontSize Lucida = (12, 22)   
fontSize Fixedsys = (8, 18)  

getRectangles :: [Figure] -> [Figure]
getRectangles = filter isRectangle
  where
    isRectangle (Rectangle _ _) = True
    isRectangle _ = False

getBound :: Figure -> Figure
getBound (Circle (x, y) r) = Rectangle (x - r, y - r) (x + r, y + r)
getBound (Rectangle p1 p2) = Rectangle p1 p2
getBound (Triangle (x1, y1) (x2, y2) (x3, y3)) = 
    let minX = minimum [x1, x2, x3]
        maxX = maximum [x1, x2, x3]
        minY = minimum [y1, y2, y3]
        maxY = maximum [y1, y2, y3]
    in Rectangle (minX, minY) (maxX, maxY)
getBound (TextBox (x, y) font text) = 
    let (width, height) = fontSize font
        totalWidth = fromIntegral (length text) * width
    in Rectangle (x, y) (x + totalWidth, y + height)

getBounds :: [Figure] -> [Figure]
getBounds = map getBound

getFigure :: [Figure] -> (Float, Float) -> Maybe Figure
getFigure [] _ = Nothing
getFigure (f:fs) point@(px, py) =
    if pointInRectangle point (getBound f) then Just f else getFigure fs point

pointInRectangle :: (Float, Float) -> Figure -> Bool
pointInRectangle (px, py) (Rectangle (x1, y1) (x2, y2)) = 
    px >= x1 && px <= x2 && py >= y1 && py <= y2
pointInRectangle _ _ = False

move :: Figure -> (Float, Float) -> Figure
move (Circle (x, y) r) (dx, dy) = Circle (x + dx, y + dy) r
move (Rectangle (x1, y1) (x2, y2)) (dx, dy) = Rectangle (x1 + dx, y1 + dy) (x2 + dx, y2 + dy)
move (Triangle (x1, y1) (x2, y2) (x3, y3)) (dx, dy) = 
    Triangle (x1 + dx, y1 + dy) (x2 + dx, y2 + dy) (x3 + dx, y3 + dy)
move (TextBox (x, y) font text) (dx, dy) = TextBox (x + dx, y + dy) font text

main :: IO ()
main = do
    let circle = Circle (0, 0) 5
    let rect = Rectangle (0, 0) (10, 10)
    let triangle = Triangle (0, 0) (5, 10) (10, 0)
    let textBox = TextBox (0, 0) Courier "Hello"
    
    print $ "Area of circle: " ++ show (area circle)
    print $ "Area of rectangle: " ++ show (area rect)
    print $ "Area of triangle: " ++ show (area triangle)
    print $ "Area of textBox: " ++ show (area textBox)
    
    print $ "Rectangles from list: " ++ show (getRectangles [circle, rect, triangle, textBox])
    
    print $ "Bounding box of triangle: " ++ show (getBound triangle)
    
    print $ "Bounding boxes of all figures: " ++ show (getBounds [circle, rect, triangle, textBox])
    
    print $ "First figure containing point (5,5): " ++ show (getFigure [circle, rect, triangle, textBox] (5, 5))
    
    print $ "Circle moved by (3, 4): " ++ show (move circle (3, 4))
