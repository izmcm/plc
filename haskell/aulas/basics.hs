fat :: Integer -> Integer
fat n | n == 0    = 1
      | otherwise = n*fat(n - 1)
           
sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList (xs)
    
doubleListRecursive :: [Int] -> [Int]
doubleListRecursive []     = []
doubleListRecursive (x:xs) = (2*x):doubleList xs

doubleList :: [Int] -> [Int]
doubleList xs = [2*a|a <- xs]

isEven :: Int -> Bool
isEven a | mod a 2 == 0   = True
         | otherwise      = False

doubleListIfEven :: [Int] -> [Int]
doubleListIfEven xs = [2*a|a <- xs, even a]

sumPairs :: [(Int, Int)] -> [Int]
sumPairs ls = [x+y|(x,y) <- ls]

memberList :: [Int] -> Int -> Bool
memberList [] x = False
memberList (a:as) x | x == a    = True
                    | otherwise = memberList as x


main = do print(fat 100)
          print(sumList [2, 1, 4, 5])
          print(doubleListRecursive [2, 1, 4, 5])
          print(doubleList [2, 1, 4, 5])
          print(isEven 6)
          print(doubleListIfEven [2, 1, 4, 5])
          print(sumPairs [(2, 1), (4, 5)])
          print(memberList [2, 1, 4, 5] 2)