data List a = Nil | Cons a (List a) deriving (Show, Eq)

-- Функция для вычисления длины списка типа List
lengthList :: List a -> Int
lengthList Nil = 0
lengthList (Cons _ xs) = 1 + lengthList xs

-- Примеры использования
main :: IO ()
main = do
    let list1 = Cons 1 (Cons 2 (Cons 3 Nil))  -- Список [1, 2, 3]
    let list2 = Nil                           -- Пустой список []
    
    print $ "Длина списка [1, 2, 3]: " ++ show (lengthList list1)
    print $ "Длина пустого списка: " ++ show (lengthList list2)
