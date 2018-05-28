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

parseLoop :: Parser Op
parseLoop = return . Loop =<< between (char '[') (char ']') (many parseAny)

parseAny :: Parser Op
parseAny = parseOp <|> parseLoop <|> (satisfy (/=']') >> parseAny)

parseProgram :: Parser Program
parseProgram = many $ try parseAny

parse :: String -> Maybe Program
parse code = do
  Right prog <- pure $ runParser parseProgram mempty code
  return prog
