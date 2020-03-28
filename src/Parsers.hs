{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Parsers (pVis, pCSV2, pCSV3) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Scientific
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Types
import Rendering

type Parser = Parsec Void Text


{- DSL Parsers -}

pVis :: Parser Vis
pVis = do
    graph <- pGraph
    colours <- pColours
    return (Vis graph colours)

pColours :: Parser [Colour]
pColours = do
    c <- pColour
    cs <- some $ do
        void (char ',')
        pColour
    return (c:cs)

pColour :: Parser Colour
pColour = choice [
    Red     <$ string "Red",
    Green   <$ string "Green",
    Blue    <$ string "Blue",
    White   <$ string "White",
    Grey    <$ string "Grey",
    Black   <$ string "Black",
    Orange  <$ string "Orange",
    Purple  <$ string "Purple"]

pGraph :: Parser Graph
pGraph = do
    gTitle <- pTitle
    void newline
    gType <- pGraphType
    gFunc <- pRenderFunction gType
    void newline
    gAxes <- pAxisLabels gType
    void newline
    gData <- some (pGraphData gType)
    return Graph{..}

pGraphType :: Parser GraphType
pGraphType = choice [
    TwoD    <$ string "2D",
    ThreeD  <$ string "3D"]

pRenderFunction :: GraphType -> Parser RenderFunction
pRenderFunction TwoD = do
        void (string "Scatter")
        return renderSquares
    <|> do
        void (string "Bar")
        return renderBars
    <|> do
        void (string "Line")
        return renderLine

pRenderFunction ThreeD = do
        void (string "Scatter")
        return renderCubes
    <|> do
        void (string "Line")
        return renderLine

pTitle :: Parser String
pTitle = do
    title <- some pStringChars
    return title

pStringChars :: Parser Char
pStringChars = alphaNumChar
    <|> char ' '

pAxisLabels :: GraphType -> Parser AxisLabels
pAxisLabels TwoD = do
    x <- pTitle
    void (char ',')
    y <- pTitle
    return $ x:(y:[])
pAxisLabels ThreeD = do
    x <- pTitle
    void (char ',')
    y <- pTitle
    void (char ',')
    z <- pTitle
    return $ x:(y:(z:[]))

pGraphData :: GraphType -> Parser GraphData
pGraphData TwoD = do
        void (string "File ")
        path <- pFilePath
        void newline
        return (File path)
    <|> do
        void (string "x:")
        xs <- pList
        void newline
        void (string "y:")
        ys <- pList
        void newline
        return $ XY (xs, ys)

pGraphData ThreeD = do
        void (string "File ")
        path <- pFilePath
        void newline
        return (File path)
    <|> do
        void (string "x:")
        xs <- pList
        void newline
        void (string "y:")
        ys <- pList
        void newline
        void (string "z:")
        zs <- pList
        void newline
        return $ XYZ (xs, ys, zs)

pList :: Parser [Float]
pList = do
    void (char '[')
    x <- L.scientific
    xs <- many pListItem
    void (char ']')
    let x' = toRealFloat x
    return (x':xs)

pListItem :: Parser Float
pListItem = do
    void (char ',')
    x <- L.scientific
    let x' = toRealFloat x
    return x'

pFilePath :: Parser String
pFilePath = do
    ps  <- some pPathComponent
    dot <- char '.'
    x   <- letterChar
    xs  <- many alphaNumChar
    return $ (concat ps) ++ [dot] ++ [x] ++ xs

pPathComponent :: Parser String
pPathComponent = do
    slash   <- char '/'
    x       <- letterChar
    xs      <- many alphaNumChar
    return $ [slash] ++ [x] ++ xs


{- Data File Parsers -}

pCSV2 :: Parser [(Float,Float)]
pCSV2 = do
    some pFloats2

pFloats2 :: Parser (Float,Float)
pFloats2 = do
    x <- L.scientific
    void (char ',')
    y <- L.scientific
    void newline
    let
        x' = toRealFloat x
        y' = toRealFloat y
    return (x', y')

pCSV3 :: Parser [(Float,Float,Float)]
pCSV3 = do
    some pFloats3

pFloats3 :: Parser (Float,Float,Float)
pFloats3 = do
    x <- L.scientific
    void (char ',')
    y <- L.scientific
    void (char ',')
    z <- L.scientific
    void newline
    let
        x' = toRealFloat x
        y' = toRealFloat y
        z' = toRealFloat z
    return (x', y', z')
