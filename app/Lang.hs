{-# LANGUAGE LambdaCase #-}
module Lang where


data Op = Dup
        | Swap
        | Toggle
        | Pass
        | NAnd
        | JNZ Program

type Program = [Op]


instance Show Op where
  show = \case
    Dup    -> ":"
    Swap   -> "/"
    Toggle -> ","
    Pass   -> "#"
    NAnd   -> "&"
    JNZ p  -> '[' : showProg p ++ "]"

showProg :: Program -> String
showProg = concatMap show

