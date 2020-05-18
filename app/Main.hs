module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Lex.Env
import UI.Artifacts
import UI.Creature
import UI.Graphemes
import UI.LexGrid
import UI.Solver

main :: IO ()
main = parseLexEnv >>= startGUI defaultConfig . setup

setup :: LexEnv -> Window -> UI ()
setup env window = do
    return window # set UI.title "Bookworm Assisstant"
    -- Block default context menu that appears when user presses right mouse button,
    -- allowing to use right mouse button in the application
    runFunction $ ffi "document.addEventListener('contextmenu', event => event.preventDefault());"
    lexTileTypeDict <- loadTileTypeImages
    (creatureEl, creatureB) <- creature env
    (artifactsEl, artifactsB) <- artifacts
    (graphemesEl, graphemesE) <- graphemes
    (tilesEl, tilesB, wildcardB) <- lexGrid lexTileTypeDict graphemesE
    solverEl <- solver env lexTileTypeDict artifactsB tilesB creatureB wildcardB
    let inputRow = row
            [ column [ element creatureEl
                     , element graphemesEl
                     , element tilesEl
                     , element artifactsEl
                     ]
            , element solverEl
            ]
    getBody window #+ [inputRow]
    return ()