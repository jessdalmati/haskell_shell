module Repl
where

import Control.Monad (unless)
import System.IO
import IOUtils
import Utils
import Commands

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
getArgs cmd = filter_ " " (tail (split ' ' cmd))

getCmd :: String -> String
getCmd cmd = head (split ' ' cmd)

execute :: String -> IO ()
execute cmd | c == "ls" = ls
            | c == "pwd" = pwd
            | c == "cd" = cd args
            | c == "cat" = cat args
            | c == "diff" = diff args
            | c == "grep" = grep args
            | c == "uniq" = uniq args
            | c == "echo" = echo args
            | c == "mkdir" = mkdir args
            | c == "" = putStrLn ""
            | otherwise = notFound c
            where 
              c = getCmd cmd 
              args = getArgs cmd

