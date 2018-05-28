module Lang where

import Data.Maybe


data Op = Dup
        | Swap
        | Toggle
        | Pass
        | NAnd
        | JNZ Program
        deriving (Show)

type Program = [Op]

-- parseOp = flip lookup
--   [ ('&', NAnd  )
--   , (':', Dup   )
--   , ('/', Swap  )
--   , (',', Toggle)
--   , ('#', Pass  ) ]

-- parseProgram = mapMaybe parseOp
