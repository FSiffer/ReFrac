module UI where

import Control.Monad (void)

import ReFrac
import Brick
import Graphics.Vty

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
handleEvent = undefined

-- Drawing
drawUI :: ReFracState -> [Widget ()]
drawUI _ = [str "Hello, world!"]

reFracAttrMap :: AttrMap
reFracAttrMap = attrMap defAttr []