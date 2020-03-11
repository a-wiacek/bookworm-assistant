module Lex.Word where
import Data.Char
import Data.Ord(Down(..))
import Data.Function(on)
import System.FilePath((</>))
import Text.Printf

data GameVersion = BWA1 | BWA2 deriving (Eq, Show)

baseDir :: GameVersion -> String
baseDir version = "words" </> show version

allWordsFile, creaturesFile, compressedFile :: String
allWordsFile = "allWords.txt"
creaturesFile = "creatures.txt"
compressedFile = "compressed.txt"

data LexGrapheme
    = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P
    | Qu | R | S | T | U | V | W | X | Y | Z
    deriving (Eq, Ord, Enum, Show)

mkLexGrapheme :: Char -> Maybe LexGrapheme
mkLexGrapheme c = lookup (toLower c) $ zip ['a' .. 'z'] [A .. Z]

lettersInGrapheme :: LexGrapheme -> Int
lettersInGrapheme Qu = 2
lettersInGrapheme _ = 1

-- LexWord represents dictionary word split into lexems.
newtype LexWord = LexWord { graphemeWord :: [LexGrapheme] } deriving (Eq, Show)

instance Ord LexWord where
    compare word1 word2 = compareWith (Down . sum . map lettersInGrapheme . graphemeWord)
                       <> compareWith graphemeWord
        where compareWith :: Ord a => (LexWord -> a) -> Ordering
              compareWith f = (compare `on` f) word1 word2

data LexTileType
    = Smashed -- Smashed is supposed to be used for all negative ailments
    -- Locked is excluded: the user is supposed to not write locked tiles
    | Basic
    | Amethyst
    | Emerald
    | Sapphire
    | Garnet
    | Ruby
    | Crystal
    | Diamond
    -- Wildcard is excluded: the user is supposed to select checkbox with wildcard
    deriving (Eq, Ord, Enum, Show)

tileTypeFilename :: LexTileType -> String
tileTypeFilename t = "images" </> case t of
    Smashed -> "smashed.jpg"
    Basic -> "basic.jpg"
    Amethyst -> "amethyst.jpg"
    Emerald -> "emerald.jpg"
    Sapphire -> "sapphire.jpg"
    Garnet -> "garnet.jpg"
    Ruby -> "ruby.jpg"
    Crystal -> "crystal.jpg"
    Diamond -> "diamond.jpg"

wildcardFilename :: String
wildcardFilename = "images" </> "rainbow.jpg"

nextTileType :: LexTileType -> LexTileType
nextTileType Diamond = Smashed
nextTileType t = succ t

allGems :: [LexTileType]
allGems = [Amethyst .. Diamond]

allTileTypes :: [LexTileType]
allTileTypes = [Smashed ..]

data LexTile
    = LexTile LexGrapheme LexTileType
    | WildcardTile LexGrapheme
    deriving (Eq, Ord, Show)

tileLexGrapheme :: LexTile -> LexGrapheme
tileLexGrapheme (LexTile g _) = g
tileLexGrapheme (WildcardTile g) = g

tileType :: LexTile -> LexTileType
tileType (LexTile _ t) = t
tileType (WildcardTile _) = Basic

mkLexTile :: Char -> LexTileType -> Maybe LexTile
mkLexTile c t = LexTile <$> mkLexGrapheme c <*> pure t

tilesToWord :: [LexTile] -> LexWord
tilesToWord = LexWord . map tileLexGrapheme

data BonusWordsCategory
    = Mammals
    | Felines
    | Bones
    | Colors
    | Fire
    | FruitsAndVegetables
    | Metals
    | Dance
    | Words
    | TomeOfTheAncientsCat
    deriving (Eq, Ord, Show, Read, Enum)

allBonusWordsCategories :: [BonusWordsCategory]
allBonusWordsCategories = [Mammals ..]

bonusWordsCategoryFilename :: BonusWordsCategory -> String
bonusWordsCategoryFilename cat = case cat of
    Mammals -> "animals.txt"
    Felines -> "bigcats.txt"
    Bones -> "bone.txt"
    Colors -> "colors.txt"
    Dance -> "dance.txt"
    Fire -> "fire.txt"
    FruitsAndVegetables -> "fruitsandvegs.txt"
    Metals -> "metals.txt"
    TomeOfTheAncientsCat -> "TomeOfTheAncients.txt"
    Words -> "words.txt"

data Creature = Creature
    { book :: Int
    , chapter :: Int
    , number :: Int
    , creatureName :: String
    } deriving (Eq, Ord)

instance Show Creature where
    show c = printf "%d.%d.%d: %s" (book c) (chapter c) (number c) (creatureName c)

creatureGameVersion :: Creature -> GameVersion
creatureGameVersion creature
    | between 1 3 = BWA1
    | between 4 6 = BWA2
    | otherwise = error $ "Invalid book number for creature " ++ show creature
    where between x y = x <= book creature && book creature <= y