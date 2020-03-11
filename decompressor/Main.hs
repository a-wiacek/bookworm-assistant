module Main where
import Data.Char
import System.FilePath

import Lex.Word

decode :: Int -> String -> String -> (String, Int)
decode no prev this = (prefix ++ suffix, no') where
    (count, suffix) = span isDigit this
    no' = if null count then no else read count
    prefix = take no' prev

decodeAll :: [String] -> [String]
decodeAll [] = []
decodeAll (h:t) = h : go 0 h t where
    go _ _ [] = []
    go no prev (this:next) = let (decoded, no') = decode no prev this
                             in decoded : go no' decoded next

hasNoQ :: String -> Bool
hasNoQ [] = True
hasNoQ ('q':'u':t) = hasNoQ t
hasNoQ ('q':_) = False
hasNoQ (_:t) = hasNoQ t

main :: IO ()
main = decodeInDir BWA1 >> decodeInDir BWA2

decodeInDir :: GameVersion -> IO ()
decodeInDir version
      = readFile (verDir </> compressedFile)
    >>= writeFile (verDir </> allWordsFile) . unlines . filter hasNoQ . decodeAll . filter (not . null) . lines
    where verDir = baseDir version
