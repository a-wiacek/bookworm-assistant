module UI.Solver
    ( solver
    ) where

import Data.Maybe (fromMaybe)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Text.Printf
import Text.Read (readMaybe)

import Lex.Artifacts
import Lex.Env
import Lex.Solver
import Lex.Word
import UI.LexGrid

singleResult :: ImgDict -> Double -> [LexTile] -> UI Element
singleResult dict score tiles =
    row $ UI.span # set UI.text (printf "%.4f" score) : map (mkUnclickableTile dict) tiles

bestWordsCounter :: UI (Element, Behavior Int)
bestWordsCounter = do
    input <- UI.input
    let inputE = fromMaybe 10 . readMaybe <$> UI.valueChange input
    finalDiv <- UI.div #+ [ UI.span # set UI.text "Show "
                          , element input # set UI.style [("width", "3em")]
                          , UI.span # set UI.text " best words (default: 10)"
                          ]
    inputB <- stepper 10 inputE
    return (finalDiv, inputB)

solver :: LexEnv
       -> ImgDict
       -> Behavior [Artifact]
       -> Behavior [LexTile]
       -> Behavior Creature
       -> Behavior Bool
       -> UI Element
solver env dict artifactsB tilesB creatureB wildcardB = do
    button <- UI.button
    (counter, counterB) <- bestWordsCounter
    set UI.children [counter] (element button)
    finalDiv <- UI.div
    wordScores <- UI.div
    wordTiles <- UI.div
    results <- row [element wordScores, element wordTiles]
    c <- column $ map element [button, results]
    set UI.children [c] (element finalDiv)
    let solverE = take
              <$> counterB
              <*> (runSolver env <$> artifactsB <*> tilesB <*> creatureB <*> wildcardB)
               <@ UI.click button
    onEvent solverE $ \sols -> do
        results' <- mapM (uncurry $ singleResult dict) sols >>= column . map element
        element wordScores # set UI.children [results']
    return finalDiv