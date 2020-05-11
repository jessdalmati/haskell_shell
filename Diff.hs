module Diff
where

import qualified Data.ByteString as L
import Data.Char (chr)

getDiff :: FilePath -> FilePath -> IO ()
getDiff p1 p2 = do
                c1 <- L.readFile p1
                c2 <- L.readFile p2
                let s1 = lineNums 1 (split '\n' (bsToStr c1))
                let s2 = lineNums 1 (split '\n' (bsToStr c2))
                let d = diff s1 s2
                putStrLn (printDiff d)

printDiff :: [(Int, String)] -> String
printDiff [] = []
printDiff (x:y:xs) = "line " ++ (show (fst x)) ++ "\n> " ++ 
       (snd x) ++ "\n> " ++ (snd y) ++ "\n\n" ++ (printDiff xs)

diff :: Eq a => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
diff _ [] = []
diff [] _ = []
diff (x:xs) (y:ys) = if snd x == snd y 
                      then diff xs ys
                      else x : y : diff xs ys

lineNums :: Int -> [String] -> [(Int, String)]
lineNums i [] = []
lineNums i ("\n":t) = (i, "\n") : lineNums i t 
lineNums i (h:t) = (i, h) : lineNums (i+1) t

split :: Char -> String -> [String]
split ch [] = [""]
split ch (h : t) | h == ch  = "" : [ch] : rest
                 | otherwise = (h : head rest) : tail rest
                 where 
                  rest = split ch t

bsToStr :: L.ByteString -> String
bsToStr = map (chr . fromEnum) . L.unpack