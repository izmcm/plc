-- [Q3] Escreva a função mapList :: (t -> t) -> List t -> List t que recebe uma função e uma lista e 
-- retorne o resultado da aplicação dessa função sobre a lista dada. 

data List t = Nil | Cons t (List t) deriving Show
                   
-- Exemplo:
-- mapList (*2) (Cons 3 (Cons 2 Nil))
-- Cons 6 (Cons 4 Nil)

mapList :: (t -> t) -> List t -> List t
mapList f Nil = Nil
mapList f (Cons c l) = (Cons (f c) (mapList f l))

main = do print(mapList (*2) (Cons 3 (Cons 2 Nil)))
