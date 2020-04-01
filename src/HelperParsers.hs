{-# LANGUAGE OverloadedStrings #-}

module HelperParsers where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Data.Scientific
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Graphics.UI.GLUT

type Parser = Parsec Void Text

pNewlineSeparatedString :: Parser String
pNewlineSeparatedString = do
    s <- some alphaNumCharWithSpace
    void newline
    return s

alphaNumCharWithSpace :: Parser Char
alphaNumCharWithSpace = alphaNumChar
    <|> char ' '

pColor :: Parser (Color4 GLfloat)
pColor = choice [
    Color4 1 0 0 1      <$ string "Red",
    Color4 0 1 0 1      <$ string "Green",
    Color4 0 0 1 1      <$ string "Blue",
    Color4 1 1 0 1      <$ string "Yellow",
    Color4 1 0.5 0 1    <$ string "Orange",
    Color4 0.5 0 0.5 1  <$ string "Purple",
    Color4 0.6 0.3 0 1  <$ string "Brown",
    Color4 1 0.75 0.8 1 <$ string "Pink",
    Color4 0 1 1 1      <$ string "Cyan"
    ]
