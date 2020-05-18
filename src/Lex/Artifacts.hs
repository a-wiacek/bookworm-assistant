module Lex.Artifacts
    ( Artifact (..)
    , allArtifacts
    , isUpgradeOf
    , isDowngradeOf
    , filterUpgrades
    , alterLetterBaseScore
    , alterTileValues
    , wordDamageMultiplier
    , categoriesMultiplier
    ) where

import Data.List (group, isInfixOf, lookup, (\\))
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

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

upgrades :: [(Artifact, Artifact)]
upgrades =
    [ (BowOfZyx, ArchOfXyzzy)
    , (TomeOfTheAncients, TabletOfTheAges)
    , (WolfbaneNecklace, SlayerTalisman)
    , (GumdropNecklace, CollapsibleIronRod)
    , (MagicPen, MagicPenCap)
    , (TheCelestialKey, SemiFlange)
    ]

isUpgradeOf :: Artifact -> Maybe Artifact
isUpgradeOf = flip lookup (map swap upgrades)

isDowngradeOf :: Artifact -> Maybe Artifact
isDowngradeOf = flip lookup upgrades

filterUpgrades :: [Artifact] -> [Artifact]
filterUpgrades artifacts = artifacts \\ mapMaybe isDowngradeOf artifacts

alterLetterBaseScore :: Artifact -> (LexGrapheme -> Int) -> LexGrapheme -> Int
alterLetterBaseScore artifact f l
    | artifact == BowOfZyx && l `elem` [X, Y, Z] = 10
    | artifact == ArchOfXyzzy && l `elem` [X, Y, Z] = 12
    | artifact == WoodenParrot && l == R = 8
    | otherwise = f l

alterTileValues :: Artifact -> (LexTile -> (Int, Double)) -> LexTile -> (Int, Double)
alterTileValues artifact f t
    | isGem && artifact `elem` [ScimitarOfJustice, GumdropNecklace] = (v, m + 0.1)
    | isGem && artifact == CollapsibleIronRod = (v, m + 0.2)
    | otherwise = (v, m)
    where (v, m) = f t
          isGem = lexTileType t `elem` allGems

wordDamageMultiplier :: [LexTile] -> Artifact -> Double
wordDamageMultiplier tiles artifact
    | artifact == QuadrumvirSignet && qua = 1.5
    | artifact == TheCelestialKey && dup = 1.5
    | artifact == SemiFlange && dup = 2
    | otherwise = 1
    where graphemes = map lexTileGrapheme tiles
          qua = [Qu, A] `isInfixOf` graphemes
          -- dup means "there are two identical consecutive letters"
          -- special case [Qu, U] is needed since there are words containing infix "quu"
          dup = any ((>1) . length) (group graphemes) || [Qu, U] `isInfixOf` graphemes

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