{- | TODO: insert header

The 'K.Initial.Initial' submodule is essentially our Prelude. It is the most
primal of KMonad's submodules, in that it may get imported by all other modules,
and may not import anything internal to KMonad. (In the category of imports, it
is our initial object.)

Additionally, as a clarifying restriction, it is probably best not to put any
code in this module that is not an import. Any definitions can live in
'K.Initial.Util', or some other submodule of 'K.Initial'.


-}
module K.Initial.Initial
  ( module X )
where

import Control.Arrow            as X (left, right)
import Control.Monad.Error.Lens as X
import Control.Lens             as X
import Control.Monad.Cont       as X
import Control.Monad.Except     as X
import Data.Acquire             as X
import GHC.Conc                 as X (orElse)
import RIO.Text                 as X (unlines, lines, unpack, pack)
import Text.Pretty.Simple       as X (pPrint, pShow)

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- The following line is required for newer stack releases.
    -- This is also the reason for the OPTIONS_GHC pragma
  , (^..), (^?), preview, (%~), (.~)

    -- Some stuff I'd rather default to Text
  , unlines, lines

    -- Will import these when I need it
  , some, many

    -- Conflicts with Control.Monad.Error.Lens
  , Handler(..), catches

    -- Often conflicts with 'try' from megaparsec
  , try
  )
