fat :: Int -> Int
fat n | n == 0    = 1
      | otherwise = n*fat(n - 1)

main = do
    putStrLn (show (fat 4))