module Parser where

import Lang (Op (..), Program)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad
import Data.Void


type Parser = Parsec Void String

parseOp :: Parser Op
parseOp = foldl1 (<|>) $ map (\(c, o) -> char c >> pure o)
  [ ('&', NAnd  )
  , (':', Dup   )
  , ('/', Swap  )
  , (',', Toggle)
  , ('#', Pass  ) ]

sc :: Parser ()
sc = void (many $ noneOf "&:/,#[]")

parseLoop :: Parser Op
parseLoop = JNZ <$> between (char '[') (char ']') parseProgram

parseAny :: Parser Op
parseAny = do
  x <- parseOp <|> parseLoop
  sc
  return x

parseProgram :: Parser Program
parseProgram = sc >> many parseAny

parse :: String -> Maybe Program
parse code = do
  Right prog <- pure $ runParser parseProgram mempty code
  return prog
