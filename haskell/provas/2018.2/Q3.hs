-- (- 3) (2.5) Outros algoritmos de compressão utilizam um "dicionário que guarda pares de códigos (inteiro)
-- e Strings, de forma que sempre que a String é reutilizada, se usa apenas o código. Para descompactar, é
-- preciso ter o dicionário e a String compactada. Implemente uma função que recebe um dicionário e uma String
-- compactada e mostre a String descompactada, isto é, sempre que aparecer um número inteiro, ele deve ser
-- substituído pela palavra no dicionário. Para simplificar o problema, assuma que os códigos tem apenas um
-- dígito.

type Dicionario = [(Int, String)]

-- exemplo: 
meuDicionario :: Dicionario
meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"
-- decode meuDicionario teste ---> "a casa tinha cafe mas nao era uma cafeteria, a casa era uma sorveteria"
-- decode :: Dicionario -> String -> String

charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

decode :: Dicionario -> String -> String
decode _ [] = ""
decode dic oStr = decodeHelp dic oStr ""

decodeHelp :: Dicionario -> String -> String -> String
decodeHelp _ [] hStr = hStr
decodeHelp dic (s:ss) hStr | s >= '0' && s <= '9' = decodeHelp dic ss (hStr ++ match (charToInt s) dic)
                           | otherwise            = decodeHelp dic ss (hStr ++ [s]) 

match :: Int -> Dicionario -> String
match _ [] = ""
match x (a:as) | x == fst a = snd a
               | otherwise  = match x as

main = do print(decode meuDicionario teste)