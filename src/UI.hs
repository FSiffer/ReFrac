module UI where

import Control.Monad (void)

import ReFrac
import Brick
import Graphics.Vty (Event( EvKey ) , Key( KChar, KEsc, KUp, KDown, KRight, KLeft), defAttr)
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
drawUI s = [str ("Hello, world at " ++ show (getX s))]

reFracAttrMap :: AttrMap
reFracAttrMap = attrMap defAttr []