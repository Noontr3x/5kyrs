data Expr = Const Float        
          | Var String         
          | Add Expr Expr      
          | Mul Expr Expr     
          | Sub Expr Expr      
          | Div Expr Expr      
          | Pow Expr Expr 
          deriving (Show, Eq)

diff :: Expr -> String -> Expr
-- Производная от константы равна 0
diff (Const _) _ = Const 0
-- Производная от переменной: если переменная совпадает с указанной, то 1, иначе 0
diff (Var v) x = if v == x then Const 1 else Const 0
-- Производная от суммы: d(f + g)/dx = df/dx + dg/dx
diff (Add f g) x = Add (diff f x) (diff g x)
-- Производная от разности: d(f - g)/dx = df/dx - dg/dx
diff (Sub f g) x = Sub (diff f x) (diff g x)
-- Производная от произведения: d(f * g)/dx = f * dg/dx + g * df/dx
diff (Mul f g) x = Add (Mul (diff f x) g) (Mul f (diff g x))
-- Производная от деления: d(f / g)/dx = (df/dx * g - f * dg/dx) / g^2
diff (Div f g) x = Div (Sub (Mul (diff f x) g) (Mul f (diff g x))) (Pow g (Const 2))
-- Производная от возведения в степень: d(f^g)/dx
diff (Pow f (Const n)) x = Mul (Mul (Const n) (Pow f (Const (n - 1)))) (diff f x)
diff (Pow f g) x = error "Не поддерживается производная от функции в общем виде"

-- Примеры использования
main :: IO ()
main = do
    let expr1 = Add (Var "x") (Const 5)        -- x + 5
    let expr2 = Mul (Var "x") (Var "x")        -- x * x
    let expr3 = Sub (Mul (Var "x") (Const 3)) (Var "y")  -- 3x - y
    let expr4 = Pow (Var "x") (Const 3)        -- x^3

    print $ "Производная от x + 5 по x: " ++ show (diff expr1 "x")
    print $ "Производная от x * x по x: " ++ show (diff expr2 "x")
    print $ "Производная от 3x - y по x: " ++ show (diff expr3 "x")
    print $ "Производная от x^3 по x: " ++ show (diff expr4 "x")
