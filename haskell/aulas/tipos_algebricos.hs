data Shape = Circle Float
             | Rectangle Float Float

isRound :: Shape -> Bool
isRound (Circle _) = True
isRound (Rectangle _ _) = False

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle h m) = h*m

-------------------------------------------------------------------------------
-- tipo type mas permite recursão (caso base Nil)   
data List t = Nil | Cons t (List t)
    deriving (Show)

toList :: List t -> [t]
toList Nil = []
toList (Cons x xs) = (x:toList xs)

fromList :: [t] -> List t
fromList [] = Nil
fromList (x:xs) = (Cons x(fromList xs))

data Tree t = NilT | Node t (Tree t) (Tree t)
    deriving (Show)

-- altura/profundidade de uma árvore
depth :: Tree t -> Int
depth NilT = 0
depth (Node pai f1 f2) = 1 + max (depth f1) (depth f2)

-- : para juntar elemento e lista
-- ++ para juntar duas listas
collapse :: Tree t -> [t]
collapse NilT = []
collapse (Node pai f1 f2) = pai:collapse f1 ++ collapse f2

-- aplica f no pai e recursivamente nos filhos
mapTree :: (t -> u) -> Tree t -> Tree u
mapTree _ NilT = NilT
mapTree f (Node pai f1 f2) = Node (f pai) (mapTree f f1) (mapTree f f2)

-- EXTRA: "xxxyywzz" -> "3x2y1w2z" (ERRADO)
encode :: String -> String
encode [] = ""
encode a = fst(connect a) ++ drop (snd connect a) a

connect :: String -> (String, Int)
connect [] = ("", 0)
connect (a:as) = (show(count a as 1) ++ [a], count a as 1)

count :: Char -> String -> Int -> Int
count a (x:xs) c | a == x    = count x xs c+1
                 | otherwise = c

main = do print(isRound (Rectangle 4.2 2.0))
          print(area (Rectangle 4.0 2.0))
          print(toList (Cons 3(Cons 4 Nil)))
          print(fromList ([1,2,3]))
          print(depth (Node 5 NilT NilT))
          print(depth (Node 5 NilT (Node 4 NilT NilT)))
          print(collapse (Node 5 NilT NilT))
          print(collapse (Node 5 NilT (Node 4 NilT NilT)))
          print(mapTree (+21) (Node 5 NilT (Node 4 NilT NilT)))
          print(encode "xxxyywzz")