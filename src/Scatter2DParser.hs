{-# LANGUAGE OverloadedStrings #-}

module Scatter2DParser (pScatter2D) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Scientific
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Graphics.UI.GLUT

import Graph
import DataSet
import HelperParsers

pScatter2D :: Parser Graph
pScatter2D = do
    axisLabels <- pAxisLabels
    d <- pDataSet
    ds <- many pDataSets
    return $ Scatter2D axisLabels ((0,0),(0,0)) (d:ds)

pAxisLabels :: Parser (String,String)
pAxisLabels = do
    xLabel <- some alphaNumCharWithSpace
    void $ char ','
    yLabel <- some alphaNumCharWithSpace
    void newline
    return (xLabel,yLabel)

pDataSets :: Parser DataSet
pDataSets = do
    void newline
    d <- pDataSet
    return d

pDataSet :: Parser DataSet
pDataSet = do
    void $ string "\nDataset:"
    label <- pNewlineSeparatedString
    dataColor <- pColor
    void newline
    pDataSet' label dataColor

pDataSet' :: String -> Color4 GLfloat -> Parser DataSet
pDataSet' label dataColor = do
        void $ string "File:"
        file <- pFileString
        return $ File label dataColor file
    <|> do
        void $ string "x:["
        xs <- pGraphData
        void $ char ']'
        void newline
        void $ string "y:["
        ys <- pGraphData
        void $ char ']'
        return $ Raw label dataColor [xs,ys]

pFileString :: Parser String
pFileString = pNewlineSeparatedString -- TODO

pGraphData :: Parser GraphData
pGraphData = do
        s <- pStringItem
        ss <- many pStringListItem
        return $ toGraphData (s:ss)
    <|> do
        x <- pFloatItem
        xs <- many pFloatListItem
        return $ toGraphData (x:xs)

pStringItem :: Parser String
pStringItem = do
    void $ char '\"'
    s <- some alphaNumChar
    void $ char '\"'
    return s

pStringListItem :: Parser String
pStringListItem = do
    void $ char ','
    s <- pStringItem
    return s

pFloatItem :: Parser Float
pFloatItem = do
    x <- L.scientific
    return $ toRealFloat x

pFloatListItem :: Parser Float
pFloatListItem = do
    void $ char ','
    x <- pFloatItem
    return x