{-# LANGUAGE RecordWildCards #-}
module Lex.Parser(parseCreatures, parseWords, throwParseError, ParseEnv) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Exit(exitFailure)
import Text.Parsec
import Text.Read

import Lex.Word

type ParseEnv = Map.Map BonusWordsCategory (Set.Set LexWord)

parseQu :: Parsec String a LexGrapheme
parseQu = (char 'q' <|> char 'Q') >> (char 'u' <|> char 'U') >> return Qu

parseLexGrapheme :: Parsec String a LexGrapheme
parseLexGrapheme
      = parseQu
    <|> (letter >>= \x -> maybe
        (fail $ "Failed to create grapheme from character " ++ [x])
        return
        (mkLexGrapheme x))
    <?> "any letter"
    
parseLexWord :: Parsec String a LexWord
parseLexWord = LexWord <$> (many1 parseLexGrapheme <* endOfLine)

parseNum :: Parsec String a Int
parseNum = read <$> many1 digit

parseLine :: Parsec String a String
parseLine = manyTill anyChar endOfLine

parseChapters :: Parsec String ParseEnv ([Creature], Map.Map Creature (Set.Set LexWord))
parseChapters = (\(x, y) -> (concat x, Map.unions y)) . unzip <$> many1 parseChapter

parseChapter :: Parsec String ParseEnv ([Creature], Map.Map Creature (Set.Set LexWord))
parseChapter = do
    bookNo <- parseNum <* char '.'
    chapterNo <- parseNum
    endOfLine
    catsWords <- mconcat <$> many (char '@' >> parseCat) 
    chapterWords <- Set.fromList <$> many parseLexWord
    fmap (fmap (Set.union (chapterWords <> catsWords)) . Map.unions) . unzip . zipWith (flip ($)) [1..]
        <$> many (parseCreature bookNo chapterNo)

parseCreature :: Int -> Int -> Parsec String ParseEnv (Int -> (Creature, Map.Map Creature (Set.Set LexWord)))
parseCreature book chapter = do
    char '!'
    creatureName <- parseLine
    catsWords <- mconcat <$> many (char '@' >> parseCat) 
    creatureWords <- Set.fromList <$> many parseLexWord
    return $ \number ->
        let creature = Creature{..}
        in (creature, Map.singleton creature $ catsWords <> creatureWords)

parseCat :: Parsec String ParseEnv (Set.Set LexWord)
parseCat = parseLine >>= \catStr -> case readMaybe catStr of
    Nothing -> fail $ "Failed to read bonus words category: " ++ catStr
    Just cat -> (Map.! cat) <$> getState

parseCreatures :: ParseEnv -> SourceName -> String
               -> Either ParseError ([Creature], Map.Map Creature (Set.Set LexWord))
parseCreatures = runParser parseChapters

parseWords :: SourceName -> String -> Either ParseError [LexWord]
parseWords = parse (many parseLexWord)

throwParseError :: Either ParseError a -> IO a
throwParseError (Left e) = putStrLn "Parse error:" >> print e >> exitFailure
throwParseError (Right x) = return x