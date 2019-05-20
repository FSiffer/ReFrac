module UI where

import Control.Monad (void)

import ReFrac
import Brick
import Graphics.Vty (white, black, red, green, Event( EvKey ) , Key( KChar, KEsc, KUp, KDown, KRight, KLeft), defAttr, Attr, rgbColor, Color)
import Control.Monad.IO.Class (liftIO)
import GHC.Word

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
    void $ defaultMain app initialState

-- Handling events
handleEvent :: ReFracState -> BrickEvent () () -> EventM () (Next ReFracState)
handleEvent s (VtyEvent (EvKey KUp []))         = continue $ move MDUp s
handleEvent s (VtyEvent (EvKey KDown []))       = continue $ move MDDown s
handleEvent s (VtyEvent (EvKey KRight []))      = continue $ move MDRight s
handleEvent s (VtyEvent (EvKey KLeft []))       = continue $ move MDLeft s
handleEvent s (VtyEvent (EvKey (KChar 'i') [])) = continue $ zoom In s
handleEvent s (VtyEvent (EvKey (KChar 'o') [])) = continue $ zoom Out s
handleEvent s (VtyEvent (EvKey (KChar 'r') [])) = continue initialState
handleEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s
handleEvent s (VtyEvent (EvKey KEsc []))        = halt s
handleEvent s _                                 = continue s

-- Drawing
drawUI :: ReFracState -> [Widget ()]
drawUI s = [(drawFractal s) <=> drawHelp ]

drawFractal :: ReFracState -> Widget ()
drawFractal s =
    Widget Fixed Fixed $ do
        ctx <- getContext
        let height = availHeight(ctx)
        let width = availWidth(ctx)

        render $ vBox $ rows height width
            where
                rows height width  = [hBox $ row width y | y <- [-quot height 2..height - 2 - quot height 2]]
                row width y = [cell x y | x <- [-quot width 2..width - 1 - quot width 2]]
                cell x y    =  withAttr (itterationToAttrName $ round $ (getFracF(s) (((fromIntegral x) + getX(s)) * getZoom(s)) (((fromIntegral y) + getY(s)) * getZoom(s)))) $ str " "
                --if x <= 240 then withAttr (itterationToAttrName x) $ str " " else str " " --

drawHelp :: Widget ()
drawHelp = str "q, esc: quit, r: reset, arrows: move, i,o: zoom"

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
itterationToColor maxItr itr = rgbColor half full half where
    half = round ( 255 * 0.5 * frac )
    full = round ( 255 * frac )
    frac = fromIntegral itr / fromIntegral maxItr
-- rgbColor (round (128 * toRational (amt * 0.5))) (round(128  * toRational amt)) (round(128  * toRational (amt * 0.5))) where amt = (realToFrac itr) / (realToFrac maxItr)
-- round $ (realToFrac maxIdx) / (realToFrac maxItr) * (realToFrac itr)