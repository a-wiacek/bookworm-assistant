{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Lex.Env
    ( GameVersionEnv
    , allWords
    , creatureSpecialWords
    , bonusCategories
    , LexEnv
    , gameSpecificEnv
    , allCreatures
    , parseLexEnv
    ) where

import Control.Monad (forM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as Text
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Lex.Parser
import Lex.Word

data GameVersionEnv = GameVersionEnv
    { allWords :: ![LexWord]
    , creatureSpecialWords :: !(Map.Map Creature (Set.Set LexWord))
    , bonusCategories :: !(Map.Map BonusWordsCategory (Set.Set LexWord))
    }

data LexEnv = LexEnv
    { gameSpecificEnv :: !(GameVersion -> GameVersionEnv)
    , allCreatures :: ![Creature]
    }

parseLexWords :: String -> IO [LexWord]
parseLexWords path = Text.readFile path >>= throwParseError . parseWords path

parseGameEnv :: GameVersion -> IO (GameVersionEnv, [Creature])
parseGameEnv version = do
    allWords <- parseLexWords allWordsPath
    bonusCategories <- fmap Map.fromList
        $ forM allBonusWordsCategories
        $ \cat -> (cat,) . Set.fromList <$> do
            p <- doesFileExist (catPath cat)
            if p then parseLexWords (catPath cat)
                 else return []
    (creatures, creatureSpecialWords) <- Text.readFile creaturesPath
        >>= throwParseError . parseCreatures bonusCategories creaturesPath
    return (GameVersionEnv{..}, creatures)
    where verDir = baseDir version
          allWordsPath = verDir </> allWordsFile
          catPath cat = verDir </> bonusWordsCategoryFilename cat
          creaturesPath = verDir </> creaturesFile

parseLexEnv :: IO LexEnv
parseLexEnv = do
    (env1, s1) <- parseGameEnv BWA1
    (env2, s2) <- parseGameEnv BWA2
    return LexEnv
        { gameSpecificEnv = \case { BWA1 -> env1; BWA2 -> env2 }
        , allCreatures = s1 ++ s2
        }