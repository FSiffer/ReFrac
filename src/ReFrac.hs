module ReFrac where

import Data.Complex

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

initialState =  ReFracState 1.0 0.0 0.0 mandelbrotFractal

-- Navigation
move :: MoveDirection -> ReFracState -> ReFracState
move MDUp s = ReFracState (getZoom s) (getX s) (getY s + 1) (getFracF s)
move MDDown s = ReFracState (getZoom s) (getX s) (getY s - 1) (getFracF s)
move MDLeft s =  ReFracState (getZoom s) (getX s + 1) (getY s) (getFracF s)
move MDRight s =  ReFracState (getZoom s) (getX s - 1) (getY s) (getFracF s)

zoom :: ZoomDirection -> ReFracState -> ReFracState
zoom In s = ReFracState (getZoom s * 0.9) (getX s) (getY s) (getFracF s)
zoom Out s = ReFracState (getZoom s * 1.1) (getX s) (getY s) (getFracF s)

-- Fractal

mandelbrot :: Num a => Int -> a -> a
mandelbrot ex a = iterate (\z -> z^ex + a) 0 !! 50

mandelbrotFractal :: Float -> Float -> Float
mandelbrotFractal x y = if magnitude (mandelbrot 2 (x :+ y)) < 2.0 then 0 else 1

notFractal :: Float -> Float -> Float
notFractal _ _ = 0
