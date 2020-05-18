module Lex.Solver
    ( runSolver
    ) where

import Data.Function (on)
import Data.List (delete, maximumBy, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set

import Lex.Artifacts
import Lex.Env
import Lex.Word

letterBaseScore :: LexGrapheme -> Int
letterBaseScore l
    | l `elem` [A, D, E, G, I, L, N, O, R, S, T, U] = 4
    | l `elem` [B, C, F, H, M, P] = 5
    | l `elem` [V, W, Y] = 6
    | l `elem` [J, K] = 7
    | l `elem` [X, Z] = 8
    | l == Qu = 11
    | otherwise = error $ "Unknown grapheme: " ++ show l -- other options are exhaustive

wordDamage :: Int -> Int
wordDamage weight = case weight `div` 4 of
    0 -> 0
    1 -> 0
    2 -> 1
    3 -> 2
    4 -> 3
    5 -> 4
    6 -> 6
    7 -> 8
    8 -> 11
    9 -> 14
    10 -> 18
    11 -> 22
    12 -> 27
    13 -> 32
    14 -> 38
    15 -> 44
    _ -> 52

-- The integer returns value of the letter.
-- Base score of the word is the sum of all values of the letters.
-- The score is used to calculate the damage of the word.
-- The double represents multiplier altering the damage.
-- Damage should be multiplied by (1 + sum of all of them).
tileBaseValues :: (LexGrapheme -> Int) -> LexTile -> (Int, Double)
tileBaseValues f t = case lexTileType t of
    Basic -> (v, 1)
    Amethyst -> (v, 1.15)
    Emerald -> (v, 1.2)
    Sapphire -> (v, 1.25)
    Garnet -> (v, 1.3)
    Ruby -> (v, 1.35)
    Crystal -> (v, 1.5)
    Diamond -> (v, 2)
    Smashed -> (0, 1)
    where v = f (lexTileGrapheme t)

-- The strategy of the function is to build word with maximum base damage.
-- To do so, it greedily takes the best possible tile for given letter.
-- Returned number gives the damage of the word.
-- Sometimes, when playing, you might want to select not the most powerful word,
-- but rather the word that removes the most ailmented tiles.

-- Games are inconsistently describing how to add multipliers:
-- BWA1 readme suggests that effects from gems, artifacts and groups stack additively.
-- BWA2 readme suggests that they stack multiplicatively (and final multiplier is rounded).
-- Since strategy from BWA1 often underestimated damage done to monster,
-- I have decided to stick to BWA2 strategy, i. e. all bonuses stack multiplicatively.

selectTiles :: [LexTile] -> Bool -> LexWord -> Maybe [LexTile]
selectTiles grid hasWildcard word = go grid hasWildcard (unLexWord word) where
    go _ _ [] = Just []
    go tiles w (h : t)
        | null l = if w then (WildcardTile h :) <$> go tiles False t else Nothing
        | otherwise = (tileUsed :) <$> go (delete tileUsed tiles) w t
        where l = filter ((==h) . lexTileGrapheme) tiles
              tileUsed = maximumBy (compare `on` lexTileType) l

computeDamage :: GameVersionEnv -> [Artifact] -> Set.Set LexWord -> [LexTile] -> Double
computeDamage env artifacts bonusWords tiles =
    let letterWeight = foldr alterLetterBaseScore letterBaseScore artifacts
        tileValues = foldr alterTileValues (tileBaseValues letterWeight) artifacts
        plum (a, b) (c, d) = (a + c, b * d)
        -- multiplier 1: crystals
        (wordValue, mul1) = foldr (plum . tileValues) (0, 1) tiles
        -- multiplier 2: artifacts basing on the structure of the word
        mul2 = foldr ((*) . wordDamageMultiplier tiles) 1 artifacts
        -- multiplier 3: artifacts basing on the category of the word
        catMultipliers = Map.fromListWith max $ concatMap categoriesMultiplier artifacts
        thisWord = lexTilesToLexWord tiles
        mulIfBelongs cat m = case Map.lookup cat (bonusCategories env) of
            Nothing -> 1
            Just words' -> if thisWord `Set.member` words'
                then m
                else 1
        mul3 = product $ Map.mapWithKey mulIfBelongs catMultipliers
        -- multiplier 4: special words for creature/chapter
        mul4 = if thisWord `Set.member` bonusWords then 4 else 1
    in fromIntegral (wordDamage wordValue) * mul1 * mul2 * mul3 * mul4

buildWord :: GameVersionEnv
          -> [Artifact]
          -> [LexTile]
          -> Set.Set LexWord
          -> Bool
          -> LexWord
          -> Maybe (Double, [LexTile])
buildWord env artifacts grid bonusWords hasWildcard word
      = (\tiles -> (computeDamage env artifacts bonusWords tiles, tiles))
    <$> selectTiles grid hasWildcard word

runSolver :: LexEnv
          -> [Artifact]
          -> [LexTile]
          -> Creature
          -> Bool
          -> [(Double, [LexTile])]
runSolver env artifacts tiles creature hasWildcard =
    let gameVersion = creatureGameVersion creature
        gameEnv = gameSpecificEnv env gameVersion
        bonusWords = fromMaybe (error $ "Bonus words not found for creature " ++ show creature)
                               (Map.lookup creature (creatureSpecialWords gameEnv))
    in sortOn (\(score, word) -> (Down score, lexTilesToLexWord word))
        $ mapMaybe (buildWord gameEnv artifacts tiles bonusWords hasWildcard) (allWords gameEnv)