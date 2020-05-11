module Commands
where

import System.IO
import System.Directory
import Utils
import IOUtils

pwd :: IO ()
pwd = do 
      wd <- getCurrentDirectory
      putStrLn wd 

cd :: [String] -> IO ()
cd [] = putStr ""
cd (x:[]) = safeSetDir x
cd (x:xs) = tooManyArgs

safeSetDir :: FilePath -> IO ()
safeSetDir fp = do 
                exists <- doesDirectoryExist fp 
                if exists 
                then setCurrentDirectory fp 
                else putStrLn "error: file path does not exist" 

ls :: IO ()
ls = do 
     contents <- getCurrentDirectory >>= listDirectory
     let outStr = prettyDirContents contents
     putStrLn outStr

cat :: [String] -> IO ()
cat [] = putStr ""
cat (x:[]) = safeCat x
cat (x:xs) = tooManyArgs

safeCat :: FilePath -> IO ()
safeCat fp = do
             exists <- doesFileExist fp
             if exists 
             then do
             fileData <- getStrContents fp
             putStrLn fileData
             else fileDoesNotExist fp 

diff :: [String] -> IO ()
diff [] = putStr ""
diff (x:y:[]) = safeDiff x y
diff (x:xs) = putStrLn "error: invalid arguments"

safeDiff :: FilePath -> FilePath -> IO ()
safeDiff p1 p2 = do
                 exists1 <- doesFileExist p1
                 exists2 <- doesFileExist p2
                 if exists1 && exists2
                 then do
                 c1 <- getStrContents p1
                 c2 <- getStrContents p2
                 let s1 = lineNums 1 (split '\n' c1)
                 let s2 = lineNums 1 (split '\n' c2)
                 let d = getDiff s1 s2
                 putStrLn (prettyPrintDiff d)
                 else putStrLn "error: file(s) not found"

grep :: [String] -> IO ()
grep [] = putStr ""
grep ("":xs) = putStrLn "error: invalid args"
grep (x:y:[]) = safeGrep x y 
grep (x:xs) = putStrLn "error: invalid args"

safeGrep :: String -> FilePath -> IO ()
safeGrep str path = do
                    exists <- doesFileExist path
                    if exists 
                    then do
                    contents <- getStrContents path
                    let grepped = grepContents str (filter_ "\n" (split '\n' contents))
                    putStrLn (printList grepped)
                    else putStrLn "error: file does not exist"

