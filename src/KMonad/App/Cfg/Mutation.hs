{-# LANGUAGE ExistentialQuantification #-}
-- |

module KMonad.App.Cfg.Mutation where

import KMonad.Prelude
import KMonad.App.Cfg.Expr
import KMonad.App.Cfg.Types
import Data.Monoid


-- mutation --------------------------------------------------------------------

data Mutation a = Mutation
  { _notes :: [Text]
  , _endo  :: Endo a
  }
makeClassy ''Mutation

instance Semigroup (Mutation a) where
  a <> b = Mutation (a^.notes <> b^.notes) (a^.endo <> b^.endo)
instance Monoid (Mutation a) where
  mempty = Mutation [] (Endo id)

-- | Create a new 'Mutation' value
mkMutation :: Text -> (s -> s) -> Mutation s
mkMutation t = Mutation [t] . Endo

-- | Create a 'Mutation' that sets some property to some value
setProp :: Text -> Traversal' s a -> a -> Mutation s
setProp t l a = mkMutation t (\s -> s & l .~ a)

-- | Lift an endo into an endo over some larger structure using a traversal
liftEndo :: Traversal' s a -> Endo a -> Endo s
liftEndo l (Endo f) = Endo $ over l f

-- option ----------------------------------------------------------------------

-- data Option a = Option
--   { _oName :: Name
--   , _oExpr :: Expr a
--   }

-- -- arg -------------------------------------------------------------------------

-- data Arg a = Arg
--   { _argName :: Name
--   , expr :: Expr a
--   }
-- makeLenses ''Arg

-- instance HasName (Arg a) where name = argName

-- flag ------------------------------------------------------------------------

-- | A datatype representing some fixed change to a structure
-- data Flag s = Flag
--   { _fName        :: Name
--   , _fDescription :: [Text]
--   , _fMutation :: Mutation s}
-- makeLenses ''Flag

-- instance HasDescription (Flag s)   where description = fDescription
-- instance HasMutation    (Flag s) s where mutation    = fMutation

-- -- | Create a flag that sets some traversal to some fixed value when triggered
-- mkFlag :: Name -> Traversal' s a -> a -> Text -> Flag s
-- mkFlag n l a d = Flag n [d] $ setProp ("flag:" <> n) l a

-- -- | Extract the change from a flag
-- runFlag :: Flag s -> Mutation s
-- runFlag = _fMutation

-- -- option ----------------------------------------------------------------------

-- | A datatype representing some settable option on a structure
data Option s = forall a. Option
  { _longName :: Name
  , _shortName :: Maybe Char
  , _expr  :: Expr a
  , _doc   :: Text
  , _mod   :: a -> Mutation s
  }
makeLenses ''Option

-- instance HasName        (Option s) where name        = oName
-- instance HasDescription (Option s) where description = oDescription

-- -- | Create an option that sets some traversal to some passed value when called
-- mkOption :: Show a => Name -> Traversal' s a -> Arg a -> Text -> Option s
-- mkOption n l a d = Option n d a
--   (\x -> mkMutation ("option:" <> n <> ":" <> tshow x) $ set l x)

-- | Create a 'Mutation' that sets some property to some value
-- setProp :: Text -> Traversal' s a -> a -> Mutation s
-- setProp t l a = mkMutation t (\s -> s & l .~ a)

mkOption :: Name -> Maybe Char -> Traversal' s a -> Expr a -> Text -> Option s
mkOption n s l e d = Option n s e d $ setProp "foo" l

options :: [Option AppCfg]
options = [
  mkOption "cfg-path" (Just 'f') cfgPath pathExpr
    "Logging verbosity: debug > info > warn > error"

        ]
