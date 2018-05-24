{-# LANGUAGE LambdaCase, MonadComprehensions, MultiWayIf #-}
module Main where

import Data.Maybe
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

data Op  = NAnd | Dup | Swap | Toggle | Pass

parseOp = flip lookup
  [ ('&', NAnd)
  , (':', Dup)
  , ('/', Swap)
  , (',', Toggle)
  , ('#', Pass)
  ]

parseProgram = mapMaybe parseOp

---------

type Memory  a = ([a], [a])
type Machine s = StateT s BitGet

getBit :: Machine (Memory Bool) Bool
getBit = do
  f <- gets fst
  if | (x:xs) <- f -> modify (first tail) >> return x
     | otherwise -> do
         e <- lift isEmpty
         if e then return False else lift getBool

putBit :: Bool -> Machine (Memory Bool) ()
putBit b = modify $ \(x,y) -> (b:x, y)

runOp = \case
  Toggle -> modify swap
  Dup    -> do a <- getBit; putBit a; putBit a
  Swap   -> do a <- getBit; b <- getBit; putBit a; putBit b
  NAnd   -> do a <- getBit; b <- getBit; putBit . not $ a && b
  Pass   -> do a <- getBit; modify $ \(l,r) -> (l, a:r)

--------

runProgram os = do
  inp <- BS.getContents
  let f = runBitGet . fmap snd . flip runStateT ([], []) $ mapM_ runOp os
  let x = runGet f inp
  let y = runPut . runBitPut . mapM_ putBool $ fst x
  -- print x
  return y

main :: IO ()
main = do
  args <- concat <$> getArgs
  code <- readFile args
  let ops = parseProgram code
  BS.putStr =<< runProgram ops
