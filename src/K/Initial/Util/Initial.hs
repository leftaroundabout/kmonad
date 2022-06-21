{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module K.Initial.Util.Initial
  ( Dt
  , us
  , ms

  , duplicates
  , whenJust
  , ifM
  , validate
  , throwEither
  )

where

import K.Initial.Initial

import qualified RIO.List     as L

-- length of time --------------------------------------------------------------

-- | A duration of time encoded as some non-negative amount of microseconds
newtype Dt = Dt { _us :: Natural }
  deriving (Num, Eq, Ord, Show, Read, Generic)
makeLenses ''Dt

-- | A lens between a non-negative amount of milliseconds and 'Dt'
ms :: Iso' Dt Natural
ms = iso (view $ us . to (`div` 1000)) (Dt . (* 1000))


-- util ------------------------------------------------------------------------

-- | Return a list of duplicate elements
--
-- This is slow and should not be used for time-critical tasks.
duplicates :: Eq a => [a] -> Maybe [a]
duplicates l = case (L.\\) l $ L.nub l of
  [] -> Nothing
  x  -> Just x

-- | Conditionally call a monadic function on 'Maybe' a value.
whenJust :: Monad m => Maybe a -> (a -> m b) -> m ()
whenJust m f = maybe (pure ()) (void . f) m

-- | Monadic if statement
ifM :: Monad m
  => m Bool -- ^ Monadic action yielding decision
  -> m a    -- ^ Action on True
  -> m a    -- ^ Action on False
  -> m a
ifM b x y = b >>= bool y x

-- | Throw some error if a validator finds a problem with some value
validate :: (MonadError e m)
  => AReview e t       -- ^ Prism from concrete error to exception
  -> (a -> Either t b) -- ^ Function that tries to find a problem in `a`
  -> a                 -- ^ The value of `a` to check
  -> m a               -- ^ The same value if no error is found
validate l f a = throwEither l . fmap (const a) $ f a

-- | Either throw some error using a prism or return a pure value.
throwEither :: (MonadError e m) => AReview e t -> Either t a -> m a
throwEither l = either (throwing l) pure
