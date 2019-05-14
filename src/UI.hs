{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (void)

import ReFrac
import Brick
import Graphics.Vty (white, black, Event( EvKey ) , Key( KChar, KEsc, KUp, KDown, KRight, KLeft), defAttr)
import Control.Monad.IO.Class (liftIO)

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
                rows height width  = [hBox $ row width y | y <- [0..height-2]]
                row width y = [cell x y | x <- [0..width-1]]
                cell x y    = str $ show $ getFracF(s) (((fromIntegral x) + getX(s))*getZoom(s)) (((fromIntegral y) + getY(s))*getZoom(s))

drawHelp :: Widget ()
drawHelp = withAttr helpAttr $ str "q, esc: quit, r: reset, arrows: move, i,o: zoom"

helpAttr :: AttrName
helpAttr = "helpAttr"

reFracAttrMap :: AttrMap
reFracAttrMap = attrMap defAttr [(helpAttr, white `on` black)]