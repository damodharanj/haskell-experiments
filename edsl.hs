plus a b = a + b

minus a b = a - b

times a b = a * b

by a b = a / b

modullo a b = a `mod` b

π a b = a + b 

x = 1 `plus` 2 `times` 3 `times` 6

y = 2 `π` 2 `times` 3 `times` 6

data Prediction = Dharan Int Int Int
data Can = Damo Int | Illa

instance Show Can where
    show (Damo x) = "damo!!" ++ show x
    show (Illa) = "Onnum illa :("

instance Show Prediction where
    show (Dharan x y z) = "Dei inga varudhe da!" ++ show (x + y + z)

data Expr
    = Val Int
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Div Expr Expr
    deriving Show

eval :: Expr -> Either String Int
eval expr = case expr of
    Val v -> pure v
    Add e1 e2 -> (+) <$> eval e1 <*> eval e2
    Mul e1 e2 -> (*) <$> eval e1 <*> eval e2
    Sub e1 e2 -> (-) <$> eval e1 <*> eval e2
    Div e1 e2 -> do
        r1 <- eval e1
        r2 <- eval e2
        if r2 == 0
        then
            Left $ "Denominator is Zero in expression: " ++ show expr
        else
            pure (r1 `div` r2)

z = (Dharan 10 20 40)

main = do
    putStrLn (show (x))
    putStrLn (show (y))
    putStrLn (show (z))
    putStrLn (show (Damo 4))
    putStrLn (show Illa)
    putStrLn (show (eval (Mul (Add (Val 1) (Val 2)) (Div (Val 8) (Val 2)))))
    putStrLn (show (foldl (*) 2 [1,2,3]))
    putStrLn (show (fmap (*2) [1,2,3]))
    putStrLn (show (fmap (*2) (Just 5)))
    putStrLn (show ((*2) <$> (Just 10))) -- maybe functor!!!
    putStrLn (show ((*2) <$> [1,2,3,4])) -- list functor!!!
    putStrLn (show ((*) <$> Just (3) <*> (Just 10))) -- maybe applicative!!!
    putStrLn (show ([(*2), (+5)] <*> [1,2,4])) -- list applicative
    putStrLn (show ((Just 3) >>= (\x -> Just (2 * x)))) -- maybe monad!!!!
    putStrLn (show ([1,2,3] >>= (\x -> [2 * x]))) -- list monad!!!!

-- Functor example

