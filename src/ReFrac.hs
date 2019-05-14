module ReFrac where

-- Type

data ReFracState = ReFracState {
    _zoom :: Float,
    _x :: Float,
    _y :: Float,
    _frac :: Float -> Float -> Float
}


-- Fractal

initialState =  ReFracState 0.0 0.0 0.0 notFractal

notFractal :: Float -> Float -> Float
notFractal _ _ = 0
