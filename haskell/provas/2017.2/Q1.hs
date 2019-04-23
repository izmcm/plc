-- 1) (2.0) Escreva uma funcao locate, que recebe como entrada um elemento e uma lista de elementos, 
-- e retorna a localização (o índice) daquele elemento dentro da lista. 

-- A primeira posição na lista tem índice 0 (zero).
-- Caso o elemento não pertença à lista, deve ser retornado o valor (-1).
-- Exemplos: locate 'x' "abcdewxyz" ------>  6
--           locate 5   [5,98,7,32] ------>  0
--           locate True [False, False] --> -1

-- locate :: Eq t => t -> [t] -> Int

locate :: Eq t => t -> [t] -> Int
locate _ [] = (-1)
locate x as = locateHelp x as (-1)

locateHelp :: Eq t => t -> [t] -> Int -> Int
locateHelp _ [] _ = (-1)
locateHelp c (a:as) x 
          | c == a    = (x+1)
          | otherwise = locateHelp c as (x+1)
          
main = do print(locate 'x' "abcdewxyz")
          print(locate 5 [5,98,7,32])
          print(locate True [False, False])