module AxisTicks where

import Graphics.UI.GLUT
import Rendering
import DataSet

type TickInfo = (GLfloat, GLfloat) -- start pos and step

{- Rendering Functions -}

renderXticks :: TickInfo -> GLfloat -> IO ()
renderXticks (start,step) offset = do
    color $ Color4 0 0 0 (1::GLfloat)
    renderLines $ generateTicks pointToTickX (start', step')
    where
        start'  = adjustTickStart (start + offset) step
        step'   = skipStepsIfSmall step 1

renderYticks :: TickInfo -> GLfloat -> IO ()
renderYticks (start,step) offset = do
    color $ Color4 0 0 0 (1::GLfloat)
    renderLines $ generateTicks pointToTickY (start', step')
    where
        start'  = adjustTickStart (start + offset) step
        step'   = skipStepsIfSmall step 1

renderZticks :: TickInfo -> GLfloat -> IO ()
renderZticks (start,step) offset = do
    color $ Color4 0 0 0 (1::GLfloat)
    renderLines $ generateTicks pointToTickZ (start', step')
    where
        start'  = adjustTickStart (start + offset) step
        step'   = skipStepsIfSmall step 1

generateTicks :: ( GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)] ) -> TickInfo -> [(GLfloat, GLfloat, GLfloat)]
generateTicks pointToTick (start,step) = concatMap (pointToTick start) steps
    where
        maxSteps = 10/step
        steps = map (*step) [0..maxSteps]

pointToTickX :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickX start x    | start+x > 0.7 || start+x < -0.7 = []
                        | otherwise = [
                            (start+x, -0.7, -0.7), (start+x, -0.68, -0.7),
                            (start+x, -0.7, -0.7), (start+x, -0.7, -0.68)
                            ]

pointToTickY :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickY start y    | start+y > 0.7 || start+y < -0.7 = []
                        | otherwise = [
                            (-0.7, start+y, -0.7), (-0.68, start+y, -0.7),
                            (-0.7, start+y, -0.7), (-0.7, start+y, -0.68)
                            ]

pointToTickZ :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
pointToTickZ start z    | start+z > 0.7 || start+z < -0.7 = []
                        | otherwise = [
                            (-0.7, -0.7, start+z), (-0.68, -0.7, start+z),
                            (-0.7, -0.7, start+z), (-0.7, -0.68, start+z)
                            ]


{- Calculating TickInfo Functions -}

calcTicks2D :: [DataSet] -> (TickInfo, TickInfo)
calcTicks2D datasets = (calcTickInfo allXs, calcTickInfo allYs)
    where
        allXs = concatMap getXdata datasets
        allYs = concatMap getYdata datasets

calcTickInfo :: [GLfloat] -> TickInfo
calcTickInfo xs = (start, step)
    where
        step    = minDifference xs
        start   = adjustTickStart (minimum xs) step

adjustTickStart :: GLfloat -> GLfloat -> GLfloat
adjustTickStart start step  | start <= (-0.7)   = start
                            | otherwise         = adjustTickStart (start-step) step

zoomTickInfo :: GLfloat -> TickInfo -> TickInfo
zoomTickInfo z (start, step) = (start', step')
    where
        step'   = z * step
        start'  = z * start


{- Helper Functions -}

minDifference :: (Num a, Ord a) => [a] -> a
minDifference xs    | length xs == 1    = 1
                    | otherwise         = minimum $ filter (\x -> x>0) $ map abs $ zipWith (-) xs (drop 1 xs)

skipStepsIfSmall :: GLfloat -> GLfloat -> GLfloat
skipStepsIfSmall 0 _ = 1.4
skipStepsIfSmall x n
    | x*n >= 0.05   = x*n
    | otherwise     = skipStepsIfSmall x (n+1)
