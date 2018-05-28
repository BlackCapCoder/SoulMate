{-# LANGUAGE MultiWayIf, LambdaCase #-}
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
import qualified Data.Set as S
import Control.Monad.IO.Class (liftIO)
import System.FilePath.Posix ((</>), splitFileName)

import Lang


type IOParsec e s = ParsecT e s IO
type Assembler    = IOParsec Void String

type Name     = String
type Argument = Name

data Atom = Plain { unplain :: String } | Todo Name deriving Show
type Code = [Atom]

data Tree = Tree
  { sections :: M.Map Name ([Argument], Code)
  } deriving Show

instance Monoid Tree where
  mempty = Tree { sections = mempty }
  mappend a b = Tree { sections = sections a `mappend` sections b }


include :: Assembler [String]
include = do
  char '#'
  space
  inc <- many $ satisfy (/='\n')
  let Right ret = runParser csv mempty $ inc ++ "\n"
  space
  return ret

csv :: Parsec Void String [String]
csv = some . L.lexeme ( do many $ oneOf " \t"
                           oneOf ",\n"
                           void . many $ oneOf " \t"
                      ) . some $ noneOf " ,\n"

loadIncludes :: S.Set String -> Assembler (S.Set String)
loadIncludes includes = do
  dir <- liftIO getCurrentDirectory
  is <- map (dir </>) . concat <$> (some include <|> return [])

  is' <- fmap concat $ liftIO $ forM is $ \i -> do
    b <- doesFileExist i
    if b then return [i]
         else filterM doesFileExist =<< map (i </>) <$> listDirectory i

  foldM (\includes' i -> do
      -- if | S.member i includes' -> return includes'
      --    | otherwise -> do
             -- liftIO $ putStrLn $ "include " ++ i
             let (pth, f) = splitFileName i
             liftIO $ setCurrentDirectory pth
             src <- liftIO $ readFile f
             setInput . (src ++) =<< getInput
             includes'' <- loadIncludes includes'
             liftIO $ setCurrentDirectory dir
             return $ S.insert i includes''
    ) includes $ S.fromList is'

assembler :: Assembler Tree
assembler = do
  loadIncludes mempty
  ss <- allSections mempty
  return ss


word :: Assembler String
word = L.lexeme space $ some alphaNumChar

junk :: Assembler ()
junk = void . many $ noneOf "&:/,#[]{}$"

symbol = L.symbol mempty

brackets :: Assembler a -> Assembler a
brackets = between (symbol "{" ) (symbol "}")


section :: Tree -> Assembler Tree
section t = do
  name <- word
  -- liftIO $ putStrLn $ "Assembling " ++ name
  args <- many word
  code <- flatten <$> brackets (body args t)
  return $ t { sections = M.insert name (args, code) $ sections t }

allSections :: Tree -> Assembler Tree
allSections t = do t' <- L.lexeme space $ section t
                   allSections t' <|> return t'

body :: [Argument] -> Tree -> Assembler Code
body args t = do
  junk
  xs <- many $ L.lexeme junk $ fmap pure code <|> macro args t
  return $ concat xs

macro :: [Argument] -> Tree -> Assembler Code
macro args t = do
  char '$'
  name <- word
  case M.lookup name $ sections t of
    Nothing | name `elem` args -> return [Todo name]
            | otherwise -> error $ name ++ " not declared"
    Just (args', code) -> do
      theargs <- count (length args') . L.lexeme space $ brackets (body args t)
      return $ inline (M.fromList $ zip args' theargs) code

code :: Assembler Atom
code = fmap Plain . some . L.lexeme junk $ oneOf "&:/,#[]"

inline :: M.Map Argument Code -> Code -> Code
inline m = concatMap $ \case
  Todo a | Just v <- M.lookup a m -> v
  x                            -> pure x

flatten :: Code -> Code
flatten (Plain a:Plain b:xs) = flatten (Plain (a++b) : xs)
flatten (x:xs) = x : flatten xs
flatten [] = []

