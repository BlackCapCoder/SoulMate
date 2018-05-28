{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf #-}
module Main where

import Data.Tuple (swap)
import Data.Binary
import Data.Binary.Get hiding (isEmpty)
import Data.Binary.Put
import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import qualified Data.ByteString.Lazy as BS
import Control.Monad.State
import Data.Function (fix)
import Control.Arrow
import System.Environment
import System.IO
import Text.Megaparsec (parseTest, runParserT, parseErrorPretty)
import qualified Data.Map as M

import Lang
import Assembler
import Parser


type Memory  a = ([a], [a])
type Machine s = StateT s BitGet

getBit :: Machine (Memory Bool) Bool
getBit = do
  f <- gets fst
  if | (x:xs) <- f -> modify (first tail) >> return x
     | otherwise -> do
         e <- lift isEmpty
         if e then return False else lift getBool

peekBit :: Machine (Memory Bool) Bool
peekBit = do
  f <- gets fst
  if | (x:xs) <- f -> return x
     | otherwise -> return False

putBit :: Bool -> Machine (Memory Bool) ()
putBit b = modify $ first (b:)

runOp = \case
  Toggle -> modify swap
  Dup    -> do a <- getBit; putBit a; putBit a
  Swap   -> do a <- getBit; b <- getBit; putBit a; putBit b
  NAnd   -> do a <- getBit; b <- getBit; putBit . not $ a && b
  Pass   -> do a <- getBit; modify $ second (a:)
  JNZ p  -> fix $ \r -> do
    b <- getBit
    when b (mapM_ runOp p >> r)

--------

runProgram os = do
  inp <- BS.getContents
  let f = runBitGet . fmap snd . flip runStateT ([], []) $ mapM_ runOp os
  let x = runGet f inp
  let y = runPut . runBitPut . mapM_ putBool $ fst x
  print x
  return y

main :: IO ()
main = do
  assint

-- interpreter = do
--   args <- unwords <$> getArgs
--   code <- readFile args
--   let ops = parseProgram code
--   BS.putStr =<< runProgram ops
--
-- assembler = do
--   args <- unwords <$> getArgs
--   code <- readFile args
--   Just out <- assemble code
--   putStr out

assint = do
  args <- unwords <$> getArgs
  code <- readFile args
  out <- runParserT assembler mempty code
  case out of
    Left err -> putStrLn $ parseErrorPretty err
    Right tree -> do
      case M.lookup "main" $ sections tree of
        Nothing -> putStrLn $ "section main not found"
        Just ([], code') -> do
          let Just ops = parse $ unplain =<< code'
          putStrLn "\nRunning code!"
          putStrLn "---------------\n"
          BS.putStr =<< runProgram ops
