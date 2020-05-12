module Utils
where 

import Text.Regex.Posix

grepLine :: String -> String -> Bool
grepLine str line = line =~ str :: Bool

grepContents :: String -> [String] -> [String]
grepContents str [] = []
grepContents str (x:xs) = if grepLine str x
                          then x : grepContents str xs
                          else grepContents str xs

filter_ :: Eq a => a -> [a] -> [a]
filter_ x [] = []
filter_ x (y:ys) = if x == y
                   then filter_ x ys
                   else y : filter_ x ys

prettyDirContents :: [FilePath] -> String
prettyDirContents contents | length contents == 0 = ""
                           | otherwise = h ++ "\n" ++ prettyDirContents t
                           where 
                              h = head contents
                              t = tail contents

printList :: [String] -> String
printList [] = ""
printList (x:[]) = x
printList (x:xs) = x ++ "\n" ++ printList xs

printEcho :: [String] -> String
printEcho [] = []
printEcho (x:[]) = x
printEcho (x:xs) = x ++ " " ++ printEcho xs

concat_ :: [String] -> String
concat_ [] = []
concat_ (x:xs) = x ++ concat_ xs

prettyPrintDiff :: [(Int, String)] -> String
prettyPrintDiff [] = []
prettyPrintDiff (x:y:xs) = "line " ++ (show (fst x)) ++ "\n> " ++ 
       (snd x) ++ "\n> " ++ (snd y) ++ "\n\n" ++ (prettyPrintDiff xs)

getDiff :: Eq a => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
getDiff _ [] = []
getDiff [] _ = []
getDiff (x:xs) (y:ys) = if snd x == snd y 
                        then getDiff xs ys
                        else x : y : getDiff xs ys

lineNums :: Int -> [String] -> [(Int, String)]
lineNums i [] = []
lineNums i ("\n":t) = (i, "\n") : lineNums i t 
lineNums i (h:t) = (i, h) : lineNums (i+1) t

testUniq :: String -> String -> Bool
testUniq a b = a == b

getUniq :: (String -> String -> Bool) -> String -> [String] -> [String]
getUniq op _ [] = []
getUniq op acc (x:xs) | op acc x = getUniq op x xs
                      | otherwise = acc : getUniq op x xs

--https://stackoverflow.com/questions/46580924/haskell-splitting-a-string-by-delimiter
split :: Char -> String -> [String]
split ch [] = [""]
split ch (h : t) | h == ch  = "" : [ch] : rest
                 | otherwise = (h : head rest) : tail rest
                 where 
                   rest = split ch t
