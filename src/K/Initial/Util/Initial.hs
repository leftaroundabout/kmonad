{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module K.Initial.Util.Initial
  ( -- * Length of time
    -- $time
    Dt
  , us
  , ms

    -- * Helpers
    -- $help
  , whenJust
  , whenNonEmpty
  , ifM
  , duplicates
  , inRIO

  , throwEither
  , devFail

    -- * Reexports
  , module K.Initial.Initial
  )
where

import K.Initial.Initial

import qualified RIO.List     as L
import qualified Control.Monad.Error.Lens as Err

-- length of time --------------------------------------------------------------

-- | A duration of time encoded as some non-negative amount of microseconds
newtype Dt = Dt { _us :: Natural }
  deriving (Num, Eq, Ord, Show, Read, Generic)
makeLenses ''Dt

-- | A lens between a non-negative amount of milliseconds and 'Dt'
ms :: Iso' Dt Natural
ms = iso (view $ us . to (`div` 1000)) (Dt . (* 1000))

-- control flow ----------------------------------------------------------------

-- | Conditionally call a monadic function on 'Maybe' a value.
whenJust :: Monad m => Maybe a -> (a -> m b) -> m ()
whenJust m f = maybe (pure ()) (void . f) m

-- | Conditionally call a monadic function on a list, only when non-empty
whenNonEmpty :: Monad m => [a] -> ([a] -> m b) -> m ()
whenNonEmpty [] _ = pure ()
whenNonEmpty xs f = void . f $ xs

-- | Monadic if statement
ifM :: Monad m
  => m Bool -- ^ Monadic action yielding decision
  -> m a    -- ^ Action on True
  -> m a    -- ^ Action on False
  -> m a
ifM b x y = b >>= bool y x

-- list helpers ----------------------------------------------------------------

-- | Return a list of duplicate elements
--
-- This is slow and should not be used for time-critical tasks.
duplicates :: Eq a => [a] -> [a]
duplicates l = (L.\\) l $ L.nub l

-- error helpers ---------------------------------------------------------------

-- | Either throw some error using a prism or return a pure value.
throwEither :: (MonadError e m) => AReview e t -> Either t a -> m a
throwEither l = either (Err.throwing l) pure

-- | Signal programmer mistake with issue-submission instructions.
devFail :: Text -> a
devFail t = error $ msg <> unpack t
  where msg = strUnlines
          [ "\nEncountered programmer error. This code should be unreachable. "
          , "Please let us know at https://github.com/kmonad/kmonad/issues "
          , "and include the following text in your submission:" ]

-- context helpers -------------------------------------------------------------

inRIO :: MonadIO m => RIO env a -> env -> m a
inRIO = flip runRIO
