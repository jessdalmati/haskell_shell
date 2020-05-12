module IOUtils
where

import System.IO
import System.Directory
import Data.ByteString as S (ByteString, unpack, readFile)
import Data.Char (chr)

tooManyArgs :: IO ()
tooManyArgs = putStrLn "error: too many arguments"

notFound :: String -> IO ()
notFound cmd = do
               let outStr = "command not found: " ++ cmd 
               putStrLn outStr

getStrContents :: FilePath -> IO String
getStrContents fp = do
                    contents <- S.readFile fp
                    return (bsToStr contents)

--https://stackoverflow.com/questions/4702325/best-way-to-convert-between-char-and-word8
bsToStr :: S.ByteString -> String
bsToStr = map (chr . fromEnum) . S.unpack

fileDoesNotExist :: FilePath -> IO ()
fileDoesNotExist fp = do
                      isDir <- doesDirectoryExist fp
                      if isDir 
                      then putStrLn ("error: " ++ fp ++ " is a directory")
                      else putStrLn "error: file does not exist"      
         
