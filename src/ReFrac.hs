module ReFrac where

-- Type

data ReFracState = ReFracState {
    getZoom :: Float,
    getX :: Float,
    getY :: Float,
    getFracF :: Float -> Float -> Float
}

data ZoomDirection
    = In
    | Out
    deriving (Eq, Show)

data MoveDirection
    = MDUp
    | MDDown
    | MDLeft
    | MDRight
    deriving (Eq, Show)

initialState =  ReFracState 0.0 0.0 0.0 notFractal

-- Navigation
move :: MoveDirection -> ReFracState -> ReFracState
move MDUp s = ReFracState (getZoom s) (getX s) (getY s - 1) (getFracF s)
move MDDown s = ReFracState (getZoom s) (getX s) (getY s + 1) (getFracF s)
move MDLeft s =  ReFracState (getZoom s) (getX s + 1) (getY s) (getFracF s)
move MDRight s =  ReFracState (getZoom s) (getX s - 1) (getY s) (getFracF s)

zoom :: ZoomDirection -> ReFracState -> ReFracState
zoom In s = ReFracState (getZoom s + 1) (getX s) (getY s) (getFracF s)
zoom Out s = ReFracState (getZoom s - 1) (getX s) (getY s) (getFracF s)

-- Fractal

notFractal :: Float -> Float -> Float
notFractal _ _ = 0
