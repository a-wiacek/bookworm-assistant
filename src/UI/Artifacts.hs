module UI.Artifacts(artifacts) where
import Data.Maybe(catMaybes)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Lex.Artifacts

mkOption :: Artifact -> UI (Element, Behavior (Maybe Artifact))
mkOption artifact = do
    option <- UI.input # set UI.type_ "checkbox"
    label <- UI.label # set UI.text (show artifact)
    let event = fmap (\b -> if b then Just artifact else Nothing) (UI.checkedChange option)
    finalDiv <- UI.new #+ [element option, element label]
    behavior <- stepper Nothing event
    return (finalDiv, behavior)

mkOptions :: [Artifact] -> UI (Element, Behavior [Artifact])
mkOptions as = do
    (options, behaviors) <- unzip <$> mapM mkOption as
    optDiv <- UI.div # set UI.children options
    return (optDiv, filterUpgrades . catMaybes <$> sequenceA behaviors)

artifacts :: UI (Element, Behavior [Artifact])
artifacts = do
    (options, behavior) <- mkOptions allArtifacts
    el <- UI.column [UI.span # set text "Select equipped artifacts:", element options]
    return (el, behavior)