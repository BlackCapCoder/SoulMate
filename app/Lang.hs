module Lang where

import Data.Maybe


data Op  = NAnd | Dup | Swap | Toggle | Pass

parseOp = flip lookup
  [ ('&', NAnd  )
  , (':', Dup   )
  , ('/', Swap  )
  , (',', Toggle)
  , ('#', Pass  ) ]

parseProgram = mapMaybe parseOp
