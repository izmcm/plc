-- 2) (3.0) Escreva uma função que verifique se uma lista está contida em outra
-- (por exemplo, se uma String é substring de outra).

-- Exemplos: substr "abc" "xyz12abrt" ----> False
--           substr "abc" "aaabrsabcfr" --> True
--           substr "aab" "aacrtxxeaayb" -> False

-- substr :: String -> String -> Bool

substr :: String -> String -> Bool
substr _ [] = False
substr sub (a:as)
          | a == sub!!0 && (take (length(sub)) (a:as)) == sub = True
          | otherwise                                         = substr sub as
          
main = do print(substr "abc" "xyz12abrt")
          print(substr "abc" "aaabrsabcfr")
          print(substr "aab" "aacrtxxeaayb") 