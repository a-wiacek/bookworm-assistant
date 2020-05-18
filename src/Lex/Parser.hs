{-# LANGUAGE RecordWildCards #-}

module Lex.Parser
    ( parseCreatures
    , parseWords
    , throwParseError
    , ParseEnv
    ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Exit (exitFailure)
import Text.Parsec
import Text.Read (readMaybe)

import Lex.Word

type ParseEnv = Map.Map BonusWordsCategory (Set.Set LexWord)

type Parser u a = Parsec Text.Text u a
type ParserEnv a = Parser ParseEnv a

parseQu :: Parser u LexGrapheme
parseQu = (char 'q' <|> char 'Q') >> (char 'u' <|> char 'U') >> return Qu

parseLexGrapheme :: Parser u LexGrapheme
parseLexGrapheme
      = parseQu
    <|> (letter >>= \x -> maybe
        (fail $ "Failed to create grapheme from character " ++ [x])
        return
        (mkLexGrapheme x))
    <?> "any letter"
    
parseLexWord :: Parser u LexWord
parseLexWord = LexWord <$> (many1 parseLexGrapheme <* endOfLine)

parseNum :: Parser u Int
parseNum = read <$> many1 digit

parseLine :: Parser u String
parseLine = manyTill anyChar endOfLine

parseChapters :: ParserEnv ([Creature], Map.Map Creature (Set.Set LexWord))
parseChapters = (\(x, y) -> (concat x, Map.unions y)) . unzip <$> many1 parseChapter

-- The schema is as follows:
-- * parseChapter parses header and then tries to parse all creatures using parseCreature
-- * parseCreature does not know by itself which number does this creature have,
--   it will be passed later
-- * it is not passed now to make use of "many" combinator
parseChapter :: ParserEnv ([Creature], Map.Map Creature (Set.Set LexWord))
parseChapter = do
    bookNo <- parseNum <* char '.'
    chapterNo <- parseNum
    endOfLine
    catsWords <- mconcat <$> many (char '@' >> parseCat) 
    chapterWords <- Set.fromList <$> many parseLexWord
    let commonWords = chapterWords <> catsWords
    (chapterCreatures, creaturesSpecialWords) <-
        unzip . zipWith (flip ($)) [1..] <$> many (parseCreature bookNo chapterNo)
    -- creaturesSpecialWords is list of Map singletons
    -- We join them into one map and add common words to each one
    return ( chapterCreatures
           , Set.union commonWords <$> Map.unions creaturesSpecialWords
           )

parseCreature
    :: Int
    -> Int
    -> ParserEnv (Int -> (Creature, Map.Map Creature (Set.Set LexWord)))
parseCreature creatureBook creatureChapter = do
    char '!'
    creatureName <- parseLine
    catsWords <- mconcat <$> many (char '@' >> parseCat) 
    creatureWords <- Set.fromList <$> many parseLexWord
    return $ \creatureNumber ->
        let creature = Creature{..}
        in (creature, Map.singleton creature $ catsWords <> creatureWords)

parseCat :: ParserEnv (Set.Set LexWord)
parseCat = parseLine >>= \catStr -> case readMaybe catStr of
    Nothing -> fail $ "Failed to read bonus words category: " ++ catStr
    Just cat -> (Map.! cat) <$> getState

parseCreatures
    :: ParseEnv
    -> SourceName
    -> Text.Text
    -> Either ParseError ([Creature], Map.Map Creature (Set.Set LexWord))
parseCreatures = runParser parseChapters

parseWords :: SourceName -> Text.Text -> Either ParseError [LexWord]
parseWords = parse (many parseLexWord)

throwParseError :: Either ParseError a -> IO a
throwParseError (Left e) = putStrLn "Parse error:" >> print e >> exitFailure
throwParseError (Right x) = return x