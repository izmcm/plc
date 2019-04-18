-- (- 1) (2.5) Um dos algoritmos mais simples de compressão de dados sem perda é run-length encoding (RLE),
-- em que sequências de dados com o mesmo valor são armazenados como um contador de repetições seguido do
-- dado. Escreva uma função encode_rle que, dada uma String, retorna uma lista de pares contendo um caractere
-- e o número de vezes que ele se repete de forma seguida na String.

-- exemplo: encode_rle "WWWWWWBWWWXYYZZZ" ---> "6W1B3W1X2Y3Z"

-- encode_rle :: String -> String

-- Pode ser usada uma estrutura de dados intermediaria / auxiliar a critério do aluno.
-- Dica: use a função show para converter um inteiro em String

encode_rle :: String -> String
encode_rle str = encodeRecursive (makeWords str) ""

encodeRecursive :: [String] -> String -> String
encodeRecursive [] str = str
encodeRecursive (a:as) str = encodeRecursive as (str ++ show(length a) ++ [a!!0])

makeWords :: String -> [String]
makeWords a = words (addSpace "" a) 

addSpace :: String -> String -> String
addSpace aux [] = aux
addSpace aux (a:as) = addSpace (aux ++ take (countTam a as 1) (a:as) ++ " ") (drop (countTam a as 1) (a:as))

countTam :: Char -> String -> Int -> Int
countTam _ [] x = x
countTam c (a:as) x | c == a = countTam a as x+1
                    | otherwise = x
                        
main = do print(encode_rle "WWWWWWBWWWXYYZZZ")