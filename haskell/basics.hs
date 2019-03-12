fat :: Integer -> Integer
fat n | n == 0    = 1
      | otherwise = n*fat(n - 1)
           
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList (xs)
    
doubleList :: [Int] -> [Int]
doubleList []     = []
doubleList (x:xs) = (2*x):doubleList xs

memberList :: [Int] -> Int -> Bool
memberList [] x = False
memberList (a:as) x | x == a    = True
                    | otherwise = memberList as x


main = do print (memberList [300, 100, 1] 1)