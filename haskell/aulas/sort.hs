-- QUICK SORT

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (a:as) = quicksort [x | x <- as, x < a] ++ [a] ++ quicksort [y | y <- as, y > a]

main = do print(quicksort [5,1,20,7,20,20])
