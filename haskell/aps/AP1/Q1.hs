-- [Q1] Escreva uma função logMes :: String -> String -> Double que recebe uma String (JAN, FEV, MAR ou ABR), 
-- uma String referente a fatura anual e retorna o total gasto no mês em questão.

-- Exemplo:
-- Main> logMes "JAN" logCartao
-- 89.4

logCartao = "14 JAN;Amazon;40.32;15 JAN;Uber;14.84;25 JAN;Uber;34.24;02 FEV;Spotify;8.50;06 FEV;Uber;6.94;05 MAR;Burger;29.90;10 MAR;Burger;24.99;15 MAR;UCI;19.00;08 ABR;Itunes;3.50;13 ABR;Picpay;20.00;"

replace :: String -> Char -> Char -> String
replace [] _ _ = ""
replace (a:as) before after
    | a == before  = after:replace as before after
    | otherwise    = a:replace as before after
    
acc :: [String] -> ([String], [String])
acc (a:a1:a2:a3:as) = ([a, a1, a2, a3], as)
    
split :: [String] -> [[String]]
split [] = []
split a  = [fst(acc a)] ++ split (snd(acc a))

toList :: String -> [[String]]
toList str = split(words(replace str ';' ' '))

logMes :: String -> String -> Double
logMes cartao mes = foldl (+) 0 [read(a!!3) ::Double | a <- toList cartao, a!!1 == mes]

main = do print(logMes logCartao "JAN")