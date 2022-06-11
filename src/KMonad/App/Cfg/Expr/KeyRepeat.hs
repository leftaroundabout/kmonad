-- |

module KMonad.App.Cfg.Expr.KeyRepeat where

import KMonad.Prelude
import KMonad.Prelude.Parsing

import KMonad.Keyboard
import KMonad.App.Cfg.Types



-- | A parser that tries to extract a 'KeyRepeatCfg' from 'Text'
--
-- Patterns
-- - sim:300:100
keyRepeatP :: Parser KeyRepeatCfg
keyRepeatP = choice
  [ string "sim:" *> (Simulate <$> (DelayRate <$> (msP <* char ':') <*> msP))
  , string "echo:" $> EchoOS
  ]

-- | Create the textual representation of a 'KeyRepeatCfg'
keyRepeatS :: KeyRepeatCfg -> KeyRepeatSpec
keyRepeatS (Simulate (DelayRate d r)) = "sim:" <> tshow (d^.ms) <> ":" <> tshow (r^.ms)
keyRepeatS EchoOS = "echo:"
