{-# LANGUAGE OverloadedStrings #-}

module Scatter3DParser (pScatter3D) where

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

pScatter3D :: Parser Graph
pScatter3D = do
    axisLabels <- pAxisLabels
    d <- pDataSet
    ds <- many pDataSets
    return $ Scatter3D axisLabels ((0,0),(0,0),(0,0)) (d:ds)

pAxisLabels :: Parser (String,String,String)
pAxisLabels = do
    xLabel <- some alphaNumCharWithSpace
    void $ char ','
    yLabel <- some alphaNumCharWithSpace
    void $ char ','
    zLabel <- some alphaNumCharWithSpace
    void newline
    return (xLabel,yLabel,zLabel)

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
        void newline
        void $ string "z:["
        zs <- pGraphData
        void $ char ']'
        return $ Raw label dataColor [xs,ys,zs]
