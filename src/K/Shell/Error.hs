-- |

module K.Shell.Error where

import K.Shell.Initial

import qualified Control.Exception.Lens as Exc

data AppError = Monkeys
makeClassyPrisms ''AppError

instance Show AppError where
  show Monkeys = "Monkeys!"

instance Exception AppError
instance AsAppError SomeException where _AppError = Exc.exception
