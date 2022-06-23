-- |

module K.Shell.Error where

import K.Shell.Initial
import Control.Monad.Catch
import qualified Control.Exception.Lens as Exc

data AppError = Monkeys
makeClassyPrisms ''AppError

instance Show AppError where
  show Monkeys = "Monkeys!"

instance Exception AppError
instance AsAppError SomeException where _AppError = Exc.exception

-- | The outermost error-catching mechanism in KMonad
--
-- This currently does nothing except rethrow the error.
handleAppError :: MonadCatch m => m a -> m a
handleAppError = Exc.handling _AppError handle
  where handle = Exc.throwing _AppError
