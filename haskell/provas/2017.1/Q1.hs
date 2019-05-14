-- 1) (2.5) Escreva uma função que verifica se uma lista já está ordenada, 
-- do menor para o maior elemento..
 
-- exemplo: isSorted [1,6,8,7,9] ------> False

-- Dica: verifique se sua resposta funciona para listas de tamanho ímpar.

-- isSorted :: Ord t => [t] -> Bool

isSorted :: Ord t => [t] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (m1:m2:ms)
   | m1 > m2   = False
   | otherwise = isSorted (m2:ms)
   
main = do print(isSorted [1,6,8,7,9])
          print(isSorted [1,6,8,9,11])
          print(isSorted [1,6,8,9])
          print(isSorted [1,10,8,9])