module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import qualified Data.Text.IO as Text
import System.FilePath ((</>))

import Lex.Word

decode :: Int -> Text -> Text -> (Text, Int)
decode no prev this = (prefix <> suffix, no') where
    (no', suffix) = case decimal this of
        Left _ -> (no, this)
        Right r -> r
    prefix = Text.take no' prev

decodeAll :: [Text] -> [Text]
decodeAll [] = []
decodeAll (h : t) = h : go 0 h t where
    go _ _ [] = []
    go no prev (this : next) = let (decoded, no') = decode no prev this
                               in decoded : go no' decoded next

hasNoQ :: Text -> Bool
hasNoQ text = case Text.uncons text of
    Nothing -> True
    Just (h1, t1) -> if h1 == 'q'
        then case Text.uncons t1 of
            Nothing -> False
            Just (h2, t2) -> h2 == 'u' && hasNoQ t2
        else hasNoQ t1

decodeInDir :: GameVersion -> IO ()
decodeInDir version
      = Text.unlines
      . filter hasNoQ
      . decodeAll
      . filter (not . Text.null)
      . Text.lines
    <$> Text.readFile (verDir </> compressedFile)
    >>= Text.writeFile (verDir </> allWordsFile)
    where verDir = baseDir version

main :: IO ()
main = decodeInDir BWA1 >> decodeInDir BWA2
