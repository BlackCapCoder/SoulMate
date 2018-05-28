module Lang where

import Data.Maybe


data Op = NAnd | Dup | Swap | Toggle | Pass
        | Loop Program -- JNZ
        deriving (Show)

type Program = [Op]

-- parseOp = flip lookup
--   [ ('&', NAnd  )
--   , (':', Dup   )
--   , ('/', Swap  )
--   , (',', Toggle)
--   , ('#', Pass  ) ]
--
-- parseProgram = mapMaybe parseOp
