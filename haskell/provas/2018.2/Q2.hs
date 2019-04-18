-- (- 2) (2.5) Escreva uma função decode_rle que descomprima uma String codificada com RLE. 

-- exemplo: decode_rle "6W1B3W1X2Y3Z" ---> "WWWWWWBWWWXYYZZZ"

-- Pode ser usada uma estrutura da dados intermediaria / auxiliar a critério do aluno.
-- Assuma que a repetição máxima é de 9 caracteres. 
-- Dica: use a função charToInt abaixo para converter um caractere numérico em um inteiro

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

-- decode_rle :: String -> String

decode_rle :: String -> String
decode_rle [] = ""
decode_rle (a1:a2:as) = take (charToInt a1) (repeat a2) ++ decode_rle as

main = do print(decode_rle "6W1B3W1X2Y3Z")