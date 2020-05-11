module Unix
where

import Control.Monad (unless)
import System.IO
import System.Directory
import Data.ByteString as S (ByteString, unpack, readFile)
import Data.Char (chr)

--https://blogg.bekk.no/creating-a-repl-in-haskell-efcdef1deec2
main :: IO ()
main = do
  input <- read_
  
  unless (input == "quit" || input == "exit")
       $ process input >> main

read_ :: IO String
read_ = putStr "REPL> "
      >> hFlush stdout
      >> getLine

process :: String -> IO ()
process cmd = execute cmd

getArgs :: String -> [String] 
getArgs cmd = removeSpaces (tail (split ' ' cmd))

removeSpaces :: [String] -> [String]
removeSpaces [] 
  = []
removeSpaces (h : t)
  | h == " " = removeSpaces t
  | otherwise = h : removeSpaces t 

getCmd :: String -> String
getCmd cmd = head (split ' ' cmd)

--https://stackoverflow.com/questions/46580924/haskell-splitting-a-string-by-delimiter
split :: Char -> String -> [String]
split ch [] = [""]
split ch (h : t) | h == ch  = "" : [ch] : rest
                 | otherwise = (h : head rest) : tail rest
                 where 
                  rest = split ch t

execute :: String -> IO ()
execute cmd | c == "ls" = ls
            | c == "pwd" = pwd
            | c == "cd" = cd args
            | c == "cat" = cat args
            | c == "" = putStrLn ""
            | otherwise = notFound c
            where 
              c = getCmd cmd 
              args = getArgs cmd

notFound :: String -> IO ()
notFound cmd = do
               let outStr = "Command not found: " ++ cmd 
               putStrLn outStr

ls :: IO ()
ls = do 
     contents <- getCurrentDirectory >>= listDirectory
     let outStr = prettyDirContents contents
     putStrLn outStr

prettyDirContents :: [FilePath] -> String
prettyDirContents contents | length contents == 0 = ""
                           | otherwise = h ++ "\n" ++ prettyDirContents t
                           where 
                              h = head contents
                              t = tail contents

pwd :: IO ()
pwd = do 
      wd <- getCurrentDirectory
      putStrLn wd 

cd :: [String] -> IO ()
cd args | args == [] = putStrLn ""
        | length args == 1 = safeSetDir (head args)
        | otherwise = tooManyArgs

safeSetDir :: FilePath -> IO ()
safeSetDir fp = do 
                exists <- doesDirectoryExist fp 
                if exists then setCurrentDirectory fp 
                  else putStrLn "error: file path does not exist" 

tooManyArgs :: IO ()
tooManyArgs = putStrLn "error: too many arguments"

cat :: [String] -> IO ()
cat args | args == [] = putStrLn ""
         | length args == 1 = safeReadFile (head args)
         | otherwise = tooManyArgs

safeReadFile :: FilePath -> IO ()
safeReadFile fp = do
                  exists <- doesFileExist fp
                  if exists then readFile_ fp
                  else fileDoesNotExist fp 

readFile_ :: FilePath -> IO ()
readFile_ fp = do
               fileData <- S.readFile fp
               putStrLn (bsToStr fileData)

--https://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8
bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack

fileDoesNotExist :: FilePath -> IO ()
fileDoesNotExist fp = do
                      isDir <- doesDirectoryExist fp
                      if isDir then putStrLn ("error: " ++ fp ++ " is a directory")
                      else putStrLn "error: file does not exist"


