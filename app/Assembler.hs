module Assembler where

import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Data.Void
import Data.Maybe
import System.Directory

import Lang


type Parser = Parsec Void String

data Tree = Tree
          { sections :: M.Map String String
          } deriving Show


symbol = L.symbol mempty

brackets :: Parser a -> Parser a
brackets = between (symbol "{" ) (symbol "}")

section :: Tree -> Parser Tree
section t = do
  space
  name <- many alphaNumChar
  space
  code <- brackets (body t)
  space
  return $ t { sections = M.insert name code $ sections t }

body :: Tree -> Parser String
body t = fmap concat . many $ macro t <|> fmap pure (satisfy (/='}'))

macro :: Tree -> Parser String
macro t = do
  char '$'
  name <- many alphaNumChar
  case M.lookup name $ sections t of
    Nothing -> guard False >> undefined
    Just x -> return x

parser :: Tree -> Parser Tree
parser t = do t' <- section t
              parser t' <|> return t'

import' :: Parser String
import' = do
  char '#'
  space
  ret <- many $ satisfy (/='\n')
  space
  return ret

assemble :: String -> IO (Maybe String)
assemble code = do
  m <- assemble' code
  return $ M.lookup "main" =<< fmap sections m

assemble' :: String -> IO (Maybe Tree)
assemble' code = do
  Right is <- pure $ runParser (many import') mempty code

  dir <- getCurrentDirectory
  ts <- forM is $ \x -> do
    let (f, pth) = let (a,b) = span (/= '/') $ reverse x in (reverse a, reverse b)
    unless (null pth) $ setCurrentDirectory pth
    r <- readFile f >>= assemble'
    setCurrentDirectory dir
    return r

  let t = Tree . foldMap sections $ catMaybes ts
  Right tree <- pure $ runParser (many import' >> (parser t <|> return t)) mempty code
  return $ return tree
