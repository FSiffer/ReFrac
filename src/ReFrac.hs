module ReFrac where

import Data.Complex

-- Type

data ReFracState = ReFracState {
    getZoom :: Double,
    getX :: Double,
    getY :: Double,
    getFracF :: Double -> Double -> Int
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
move MDUp s = ReFracState (getZoom s) (getX s) (getY s - getZoom s) (getFracF s)
move MDDown s = ReFracState (getZoom s) (getX s) (getY s + getZoom s) (getFracF s)
move MDLeft s =  ReFracState (getZoom s) (getX s - getZoom s) (getY s) (getFracF s)
move MDRight s =  ReFracState (getZoom s) (getX s + getZoom s) (getY s) (getFracF s)

zoom :: ZoomDirection -> ReFracState -> ReFracState
zoom In s = ReFracState (getZoom s * 0.9) (getX s) (getY s) (getFracF s)
zoom Out s = ReFracState (getZoom s * 1.1) (getX s) (getY s) (getFracF s)

-- Fractal
max_itterations = 240

mandelbrot :: Num a => Int -> a -> a
mandelbrot ex a = iterate (\z -> z^ex + a) 0 !! 50

mandelbrotFractal :: Double -> Double -> Int
mandelbrotFractal x y = length . takeWhile (\z -> magnitude z <= 2) . take max_itterations $ iterate (\z -> z^2 + (x :+ y)) 0

notFractal :: Double -> Double -> Int
notFractal _ _ = 0
