module Lex.Artifacts where
import Data.List(group, isInfixOf, (\\))
import Data.Maybe

import Lex.Word

-- Only artifacts that could alter choice of words appear here
-- (so no Hephaestus' Hammer because it gives 2 quarters of damage to all words)

data Artifact
    = BowOfZyx -- X, Y and Z are worth 10 quarters
    | ArchOfXyzzy -- X, Y and Z are worth 12 quarters
    | HandOfHercules -- 50% bonus for metal words, (+4 quarters of damage ignored - same bonus for all words)
    | TomeOfTheAncients -- 100% for color words
    | WoodenParrot -- R is worth 8 quarters
    | TabletOfTheAges -- 150% for color words
    | ScimitarOfJustice -- 10% to all gems
    | WolfbaneNecklace -- 50% for mammal words
    | QuadrumvirSignet -- 50% for 'QUA' words
    | SlayerTalisman -- 75% for mammal words
    -- Artifacts related to BWA2 are less detailed:
    -- MacFarmer's Almanac/Master's Zodiac are not implemented
    -- No detailed info on artifacts (values in code below are guessed)
    | GumdropNecklace -- gem effects slightly increased
    | CollapsibleIronRod -- gem effects increased
    | TheCelestialKey -- ?% for double-letter words
    | MagicPen -- ?% for 'words' words
    | MagicPenCap -- ?% for 'words' words
    | SemiFlange -- ?% for double-letter words
    deriving (Eq, Enum)

instance Show Artifact where
    show BowOfZyx = "Bow of Zyx"
    show ArchOfXyzzy = "Arch of Xyzzy"
    show HandOfHercules = "Hand of Hercules"
    show TomeOfTheAncients = "Tome of the Ancients"
    show WoodenParrot = "Wooden Parrot"
    show TabletOfTheAges = "Tablet of the Ages"
    show ScimitarOfJustice = "Scimitar of Justice"
    show WolfbaneNecklace = "Wolfbane Necklace"
    show QuadrumvirSignet = "Quadrumvir Signet"
    show SlayerTalisman = "Slayer Talisman"
    show GumdropNecklace = "Gumdrop Necklace"
    show CollapsibleIronRod = "Collapsible Iron Rod"
    show TheCelestialKey = "The Celestial Key"
    show MagicPen = "Magic Pen"
    show MagicPenCap = "Magic Pen Cap"
    show SemiFlange = "SemiFlange"

allArtifacts :: [Artifact]
allArtifacts = [BowOfZyx ..]

isUpgradeOf :: Artifact -> Maybe Artifact
isUpgradeOf ArchOfXyzzy = Just BowOfZyx
isUpgradeOf TabletOfTheAges = Just TomeOfTheAncients
isUpgradeOf SlayerTalisman = Just WolfbaneNecklace
isUpgradeOf CollapsibleIronRod = Just GumdropNecklace
isUpgradeOf MagicPenCap = Just MagicPen
isUpgradeOf SemiFlange = Just TheCelestialKey
isUpgradeOf _ = Nothing

isDowngradeOf :: Artifact -> Maybe Artifact
isDowngradeOf BowOfZyx = Just ArchOfXyzzy
isDowngradeOf TomeOfTheAncients = Just TabletOfTheAges
isDowngradeOf WolfbaneNecklace = Just SlayerTalisman
isDowngradeOf GumdropNecklace = Just CollapsibleIronRod
isDowngradeOf MagicPen = Just MagicPenCap
isDowngradeOf TheCelestialKey = Just SemiFlange
isDowngradeOf _ = Nothing

filterUpgrades :: [Artifact] -> [Artifact]
filterUpgrades artifacts = artifacts \\ mapMaybe isDowngradeOf artifacts

alterLetterBaseScore :: Artifact -> (LexGrapheme -> Int) -> LexGrapheme -> Int
alterLetterBaseScore BowOfZyx f l | l `elem` [X, Y, Z] = 10
                                  | otherwise = f l
alterLetterBaseScore ArchOfXyzzy f l | l `elem` [X, Y, Z] = 12
                                     | otherwise = f l
alterLetterBaseScore WoodenParrot f l | l == R = 8
                                      | otherwise = f l
alterLetterBaseScore _ f l = f l

alterTileValues :: Artifact -> (LexTile -> (Int, Double)) -> LexTile -> (Int, Double)
alterTileValues ScimitarOfJustice f t
    | tileType t `elem` allGems = (v, m + 0.1)
    | otherwise = (v, m)
    where (v, m) = f t
alterTileValues GumdropNecklace f t
    | tileType t `elem` allGems = (v, m + 0.1)
    | otherwise = (v, m)
    where (v, m) = f t
alterTileValues CollapsibleIronRod f t
    | tileType t `elem` allGems = (v, m + 0.2)
    | otherwise = (v, m)
    where (v, m) = f t
alterTileValues _ f t = f t

wordDamageMultiplier :: [LexTile] -> Artifact -> Double
wordDamageMultiplier tiles QuadrumvirSignet
    | [Qu, A] `isInfixOf` map tileLexGrapheme tiles = 1.5
    | otherwise = 1
wordDamageMultiplier tiles TheCelestialKey
    | any ((>1) . length) (group $ map tileLexGrapheme tiles) = 1.5
    | otherwise = 1
wordDamageMultiplier tiles SemiFlange
    | any ((>1) . length) (group $ map tileLexGrapheme tiles) = 2
    | otherwise = 1
wordDamageMultiplier _ _ = 1

categoriesMultiplier :: Artifact -> [(BonusWordsCategory, Double)]
categoriesMultiplier artifact = case artifact of
    HandOfHercules -> [(Metals, 1.5)]
    TomeOfTheAncients -> [(Colors, 2)]
    TabletOfTheAges -> [(Colors, 2.5)]
    WolfbaneNecklace -> [(Mammals, 1.5)]
    SlayerTalisman -> [(Mammals, 1.75)]
    MagicPen -> [(Words, 2)]
    MagicPenCap -> [(Words, 2.5)]
    _ -> []