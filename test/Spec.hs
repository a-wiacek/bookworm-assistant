import Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.FilePath
import Test.Hspec

import Lex.Parser
import Lex.Word

isSorted :: Ord a => [a] -> Bool
isSorted (x:y:z) = x < y && isSorted (y:z)
isSorted _ = True

runForBoth :: (GameVersion -> Expectation) -> Expectation
runForBoth test = test BWA1 >> test BWA2

emptyEnv :: ParseEnv
emptyEnv = Map.fromList [(cat, Set.empty) | cat <- allBonusWordsCategories]

main :: IO ()
main = hspec $ do
    describe "allWords.txt" $
        it "all words should be in lexicographical order" $ runForBoth $ \version -> do
            content <- lines <$> readFile (baseDir version </> allWordsFile)
            isSorted content `shouldBe` True
    describe "parsing characters.txt" $
        it "characters.txt files should be parsed properly" $ runForBoth $ \version -> do
            content <- readFile (baseDir version </> creaturesFile)
            parseCreatures emptyEnv "" content `shouldSatisfy` isRight