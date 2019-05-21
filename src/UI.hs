module UI where

import Control.Monad (void)

import ReFrac
import Brick
import Graphics.Vty 
import Control.Monad.IO.Class (liftIO)
import GHC.Word
import Text.Printf
import Data.Fixed
import Brick.BChan

-- Types

-- App definition
app :: App ReFracState () ()
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const reFracAttrMap
          }

main :: IO ()
main = do
    eventChan <- Brick.BChan.newBChan 1
    let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
    initialVty <- buildVty
    void $ customMain initialVty buildVty
        (Just eventChan) app initialState
    -- Use finalState and exit

-- Handling events
handleEvent :: ReFracState -> BrickEvent () () -> EventM () (Next ReFracState)
handleEvent s (VtyEvent (EvKey KUp []))         = continue $ move MDUp s
handleEvent s (VtyEvent (EvKey KDown []))       = continue $ move MDDown s
handleEvent s (VtyEvent (EvKey KRight []))      = continue $ move MDRight s
handleEvent s (VtyEvent (EvKey KLeft []))       = continue $ move MDLeft s
handleEvent s (VtyEvent (EvKey (KChar 'i') [])) = continue $ zoom In s
handleEvent s (VtyEvent (EvKey (KChar 'o') [])) = continue $ zoom Out s
handleEvent s (VtyEvent (EvKey (KChar 'k') [])) = continue $ changeEx Inc s
handleEvent s (VtyEvent (EvKey (KChar 'l') [])) = continue $ changeEx Dec s
handleEvent s (VtyEvent (EvKey (KChar 'r') [])) = continue initialState
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s
handleEvent s (VtyEvent (EvKey KEsc []))        = halt s
handleEvent s _                                 = continue s

-- Drawing
drawUI :: ReFracState -> [Widget ()]
drawUI s = [(drawFractal s) <=> (hBox [drawHelp, fill ' ', drawStateInfo s])]

drawFractal :: ReFracState -> Widget ()
drawFractal s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let height = availHeight(ctx)
        let width = availWidth(ctx)

        render $ vBox $ rows height width
            where
                rows height width  = [hBox $ row width y | y <- [-quot height 2..height - 2 - quot height 2]]
                row width y = [cell x y | x <- [-quot width 2..width - 1 - quot width 2]] --[0..width - 1]]
                cell x y    =  withAttr (itterationToAttrName $ (getFracF(s) (getEx s) (getX(s) + (fromIntegral x) * getZoom(s)) (getY(s) + (fromIntegral y) * getZoom(s)))) $ str " "
                --cell x y    =  withAttr (itterationToAttrName $ round (getX s) + x) $ str " "

drawHelp :: Widget ()
drawHelp = str "q, esc: quit, r: reset, arrows: move, i,o: zoom, k,l: change exp."

drawStateInfo :: ReFracState -> Widget ()
drawStateInfo s = str $ printf "exp.: %d, zoom: %.4e, pos: (%.4e,%.4e)" (getEx s) (getZoom s) (getX s) (getY s)

helpAttr :: AttrName
helpAttr = attrName "helpAttr"

reFracAttrMap :: AttrMap
reFracAttrMap = attrMap defAttr $ [(helpAttr, white `on` black)] ++ fracColors

-- Color Mapping

fracColors :: [(AttrName, Attr)]
fracColors = (itterationToAttrName max_itterations, bg black)
    : [(itterationToAttrName r, bg $ itterationToColor max_itterations r) | r <- [1 .. max_itterations-1]]
-- 0 itterations is never returned

itterationToAttrName :: Int -> AttrName
itterationToAttrName itr = attrName $ "fracColor_" ++ show itr

itterationToColor :: Int -> Int -> Color
itterationToColor maxItr itr = toColor (hsvToRGB (hue, 1.0, 1.0)) where --Color240 (fromIntegral itr) where -- rgbColor half full half where
    hue = (fromIntegral itr / fromIntegral maxItr) * 360
-- rgbColor (round (128 * toRational (amt * 0.5))) (round(128  * toRational amt)) (round(128  * toRational (amt * 0.5))) where amt = (realToFrac itr) / (realToFrac maxItr)
-- round $ (realToFrac maxIdx) / (realToFrac maxItr) * (realToFrac itr)

toColor :: RGB -> Color
toColor (r, g, b) = rgbColor r g b

type HSV = (Double, Double, Double)
type RGB = (Int, Int, Int)

-- hue [0, 360]; saturation [0, 1]; value [0, 1]
hsvToRGB :: HSV -> RGB
hsvToRGB (h, s, v) = (r 5, r 3, r 1)
    where
        r n = ceiling $ (f n) * 255
        f n = v - v * s * max (minimum [k, 4 - k, 1]) 0
            where
                k = (n + (h / 60)) `mod'` 6
