{-# LANGUAGE FlexibleInstances #-}

module GraphData where

import Graphics.UI.GLUT

-- numerical data + string representation
type GraphData = ([GLfloat], [String])

class Graphable a where
    toGraphData :: a -> GraphData
    toGraphData a = ([],[])

instance Graphable [GLfloat] where
    toGraphData xs = (xs,ss)
        where
            ss = map show xs

instance Graphable [String] where
    toGraphData ss = (xs,ss)
        where
            xs = take (length ss) [1..]::[GLfloat]
