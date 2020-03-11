module UI.LexGrid(ImgDict, wildcardImg, loadTileTypeImages, lexGrid, mkUnclickableTile) where
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Text.Printf

import Lex.Word

data ImgDict = ImgDict 
    { _tileTypeImg :: Map.Map LexTileType String
    , wildcardImg :: String
    }

formatImgUrl :: String -> String
formatImgUrl = printf "url('%s')"

tileTypeImg :: ImgDict -> LexTileType -> String
tileTypeImg imgDict t = formatImgUrl (_tileTypeImg imgDict Map.! t)

loadTileTypeImage :: LexTileType -> UI (LexTileType, String)
loadTileTypeImage t = do
    s <- loadFile "image/jpeg" (tileTypeFilename t)
    return (t, s)

loadTileTypeImages :: UI ImgDict
loadTileTypeImages
      = ImgDict . Map.fromList
    <$> mapM loadTileTypeImage allTileTypes
    <*> loadFile "image/jpeg" wildcardFilename

setTileStyle :: ImgDict -> Maybe LexTileType -> UI Element -> UI Element
setTileStyle dict t = set UI.style
    [ ("background-image", img)
    , ("height", size)
    , ("width", size)
    , ("line-height", size)
    , ("font-size", "2em")
    , ("text-align", "center")
    , ("border", "thin solid black")
    ] where size = "50px"
            img = maybe (formatImgUrl $ wildcardImg dict) (tileTypeImg dict) t

mkClickableTile :: ImgDict -> Event a -> UI (Element, Behavior LexTileType)
mkClickableTile dict resetE = do
    box <- UI.div # setTileStyle dict (Just Basic)
    eType <- accumE Basic $ foldr1 (unionWith const) -- first event in the list has the highest priority
        [ const Basic <$ resetE -- changing graphemes resets tile type
        , const Basic <$ UI.contextmenu box -- right click resets tile type
        , nextTileType <$ UI.click box -- left click chooses next tile type
        ]
    bType <- stepper Basic eType
    onEvent eType $ \t -> element box # setTileStyle dict (Just t)
    return (box, bType)

mkUnclickableTile :: ImgDict -> LexTile -> UI Element
mkUnclickableTile dict tile = UI.div
    # setTileStyle dict (case tile of { WildcardTile{} -> Nothing; _ -> Just (tileType tile) })
    # set UI.text (show $ tileLexGrapheme tile)

freezeTiles :: UI (Element, Behavior Bool)
freezeTiles = do
    option <- UI.input # set UI.type_ "checkbox"
    label <- UI.label # set UI.text "Do not reset tile types"
    behavior <- stepper True (not <$> UI.checkedChange option)
    finalDiv <- UI.new #+ [element option, element label]
    return (finalDiv, behavior)

wildcard :: UI (Element, Behavior Bool)
wildcard = do
    option <- UI.input # set UI.type_ "checkbox"
    label <- UI.label # set UI.text "Use one wildcard"
    behavior <- stepper False (UI.checkedChange option)
    finalDiv <- UI.new #+ [element option, element label]
    return (finalDiv, behavior)

-- Input list has length at most 16
mkTiles :: ImgDict -> Event [LexGrapheme] -> UI (Element, Behavior [LexTileType], Behavior Bool)
mkTiles dict graphemesE = do
    (freezeTilesCheckBox, resetB) <- freezeTiles
    (wildcardCheckBox, wildcardB) <- wildcard
    (boxes, typesB) <- fmap sequenceA . unzip
                   <$> replicateM 16 (mkClickableTile dict $ whenE resetB graphemesE)
    let squareList l = map (\x -> take 4 $ drop x l) [0, 4, 8, 12]
    boxesGrid <- grid $ map (map element) (squareList boxes)
    onEvent graphemesE $ \gs ->
        let emptyBoxes = drop (length gs) boxes
        in zipWithM_ (set UI.text) (map show gs) (map element boxes)
            >> mapM_ (set UI.text "" . element) emptyBoxes
    finalDiv <- UI.new #+ [element freezeTilesCheckBox, element boxesGrid, element wildcardCheckBox]
    return (finalDiv, typesB, wildcardB)

lexGrid :: ImgDict -> Event [LexGrapheme] -> UI (Element, Behavior [LexTile], Behavior Bool)
lexGrid dict graphemesE = do
    graphemesB <- stepper [] graphemesE
    (boxesGrid, typesB, wildcardB) <- mkTiles dict graphemesE
    return (boxesGrid, zipWith LexTile <$> graphemesB <*> typesB, wildcardB)