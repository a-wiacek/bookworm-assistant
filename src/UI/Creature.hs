{-# LANGUAGE ScopedTypeVariables #-}
module UI.Creature(creature) where
import qualified Data.Map.Strict as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Lex.Env
import Lex.Word

mkCreature :: Creature -> UI Element
mkCreature c = UI.option # set UI.text (show c)

creature :: LexEnv -> UI (Element, Behavior Creature)
creature env = do
    list <- UI.select
    next <- UI.button # set UI.text "Next"
    let creaturesIndex = Map.fromList $ zip [0 :: Int ..] allC
        maxSel = length allC - 1
        selectE = const <$> filterJust (UI.selectionChange list)
        nextE = (\s -> min maxSel (s + 1)) <$ UI.click next
    creatureIndexE <- accumE 0 (unionWith const selectE nextE)
    let creatureE = filterJust (flip Map.lookup creaturesIndex <$> creatureIndexE)
    creatureB <- stepper (head allC) creatureE

    onEvent creatureIndexE $ \s -> element list # set UI.selection (Just s)
    options <- mapM mkCreature allC
    element list # set UI.children options
    finalEl <- column [ row [ UI.span # set text "Select enemy creature:"
                            , element next
                            ]
                      , element list
                      ]
    return (finalEl, creatureB)
    where allC = allCreatures env