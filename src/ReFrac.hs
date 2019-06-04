module ReFrac where

import Data.Complex

-- Type

data ReFracState = ReFracState {
    getZoom :: Double,
    getX :: Double,
    getY :: Double,
    getEx :: Int,
    getFracF :: Int -> Double -> Double -> Int
}

data ZoomDirection
    = In
    | Out
    deriving (Eq, Show)

data ChangeEx
    = Inc
    | Dec
    deriving (Eq, Show)

data MoveDirection
    = MDUp
    | MDDown
    | MDLeft
    | MDRight
    deriving (Eq, Show)

initialState =  ReFracState 1.0 0.0 0.0 2 mandelbrotItterations

-- Navigation
move :: MoveDirection -> ReFracState -> ReFracState
move MDUp s = ReFracState (getZoom s) (getX s) (getY s - getZoom s) (getEx s) (getFracF s)
move MDDown s = ReFracState (getZoom s) (getX s) (getY s + getZoom s) (getEx s) (getFracF s)
move MDLeft s =  ReFracState (getZoom s) (getX s - getZoom s) (getY s) (getEx s) (getFracF s)
move MDRight s =  ReFracState (getZoom s) (getX s + getZoom s) (getY s) (getEx s) (getFracF s)

zoom :: ZoomDirection -> ReFracState -> ReFracState
zoom In s = ReFracState (getZoom s * 0.9) (getX s) (getY s) (getEx s) (getFracF s)
zoom Out s = ReFracState (getZoom s * 1.1) (getX s) (getY s) (getEx s) (getFracF s)

changeEx :: ChangeEx -> ReFracState -> ReFracState
changeEx Inc s = ReFracState (getZoom s) (getX s) (getY s) (getEx s + 1) (getFracF s)
changeEx Dec s = if getEx s > 0 then ReFracState (getZoom s) (getX s) (getY s) (getEx s - 1) (getFracF s) else s

-- Fractal
max_itterations = 240

mandelbrot :: Int -> Double -> Double -> (Complex Double -> Complex Double)
mandelbrot ex x y = \z -> z^ex + (x :+ y)

mandelbrotItterations :: Int -> Double -> Double -> Int
mandelbrotItterations ex x y = length . takeWhile (\z -> absSqr z <= 4) . take max_itterations $ iterate (mandelbrot ex x y) 0

absSqr :: Complex Double -> Double
absSqr (re :+ im) = re * re + im * im

notFractal :: Int -> Double -> Double -> Int
notFractal _ _ _ = 0
