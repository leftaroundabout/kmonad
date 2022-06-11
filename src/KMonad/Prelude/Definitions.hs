{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |

module KMonad.Prelude.Definitions
  ( Name
  , Names
  , Named
  , NameError
  , HasName(..)
  , HasNames(..)
  , AsNameError(..)
  , checkNames
  , checkNamesThrow
  , nameFor

  , Dt
  , us
  , ms

  , duplicates
  , validate
  , throwEither
  )
where

import KMonad.Prelude.Imports

import qualified RIO.List     as L
import qualified RIO.Text     as T



--------------------------------------------------------------------------------

-- | A duration of time encoded as some non-negative amount of microseconds
newtype Dt = Dt { _us :: Natural }
  deriving (Num, Eq, Ord, Show, Read, Generic)
makeLenses ''Dt

-- | A lens between a non-negative amount of milliseconds and 'Dt'
ms :: Iso' Dt Natural
ms = iso (view $ us . to (`div` 1000)) (Dt . (* 1000))


--------------------------------------------------------------------------------

-- | Return a list of duplicate elements
--
-- This could be faster but is never really used for time-critical or large tasks.
duplicates :: Eq a => [a] -> Maybe [a]
duplicates l = case (L.\\) l $ L.nub l of
  [] -> Nothing
  x  -> Just x

--------------------------------------------------------------------------------

-- | Text in its function as a symbol for some other value
type Name = Text
type Names = [Name]
type Named a = [(Name, a)]

-- | Things that can go wrong with 'Name' or 'Names'
data NameError
  = EmptyName            -- ^ Encountered an empty 'Name'
  | DuplicateNames Names -- ^ Encountered duplicate 'Names'
  deriving Eq
makeClassyPrisms ''NameError

instance Show NameError where
  show EmptyName = "Encountered an empty <Name>"
  show (DuplicateNames ns) = "Encountered duplicate names: "
    <> (unpack . T.intercalate ", " . map tshow $ ns)

instance Exception NameError
instance AsNameError SomeException where _NameError = exception

class HasName a where name :: Lens' a Name
class HasNames a where names :: Fold a Name

instance HasName Name where name = id
instance HasNames Name where names = id
instance HasNames Names where names = folded
instance HasNames (Named a) where names = folded . _1

-- | Check a collection of names for problems
--
-- A collection of names can be broken if:
-- - Any of the names is ""
-- - Any of the names are duplicates
checkNames :: HasNames a => a -> Maybe NameError
checkNames a = let ns = a^..names in case L.find T.null ns of
  Just _  -> Just EmptyName
  Nothing -> DuplicateNames <$> duplicates ns

-- | Like `checkNames` but throws the error if encountered
checkNamesThrow :: HasNames a => a -> a
checkNamesThrow = validate _NameError checkNames

-- | Do a reverse-lookup for the name of some item
nameFor :: Eq a => a -> Named a -> Maybe Name
nameFor v = lookup v . map (view swapped)

--------------------------------------------------------------------------------

-- | Throw some error if a validator finds a problem with some value
validate :: ()
  => AReview SomeException b -- ^ Prism from concrete error to exception
  -> (a -> Maybe b)          -- ^ Function that tries to find a problem in `a`
  -> a                       -- ^ The value of `a` to check
  -> a                       -- ^ The same value if no error is found
validate p f a = maybe a (throwing p) . f $ a

-- | Use a prism to 'SomeException' to either throw an error or return success.
throwEither :: AReview SomeException e -> Either e a -> a
throwEither p = either (throwing p) id
