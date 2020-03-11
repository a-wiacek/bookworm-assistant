module UI.Graphemes(graphemes) where
import Data.Maybe
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Lex.Word

graphemes :: UI (Element, Event [LexGrapheme])
graphemes = do
    lettersEntry <- UI.input
    let graphemesE = mapMaybe mkLexGrapheme . take 16 <$> UI.valueChange lettersEntry
    el <- UI.column [ UI.span # set text "Write your letters (q only for Qu tile):"
                    , element lettersEntry
                    ]
    return (el, graphemesE)
