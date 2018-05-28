{-# LANGUAGE LambdaCase #-}
module Lang where

import Data.Maybe


data Op = Dup
        | Swap
        | Toggle
        | Pass
        | NAnd
        | JNZ Program

instance Show Op where
  show = \case
    Dup -> ":"
    Swap -> "/"
    Toggle -> ","
    Pass -> "#"
    NAnd -> "&"
    JNZ p -> '[' : showProg p ++ "]"

showProg :: Program -> String
showProg = concatMap show

type Program = [Op]

-- parseOp = flip lookup
--   [ ('&', NAnd  )
--   , (':', Dup   )
--   , ('/', Swap  )
--   , (',', Toggle)
--   , ('#', Pass  ) ]

-- parseProgram = mapMaybe parseOp
