-- |

module K.Shell.Cfg.Expr.Initial
  ( ExprType
  , Expr
  , ExprError
  , AsExprError(..)

  , encode
  , decode

  , namedExpr
  , customExpr

  , exprTail

  , module K.Initial
  , module K.Initial.Parsing
  , module K.Shell.Cfg.Initial
  )
where

import K.Initial
import K.Initial.Parsing
import K.Shell.Cfg.Initial

import qualified Control.Monad.Error.Lens as Err

-- basic types -----------------------------------------------------------------

-- | An informative name describing what an 'Expr' should evaluate to.
type ExprType = Text

-- | An 'Expr', describing how some value gets encoded as raw text
data Expr a = Expr
  { _exprType :: ExprType
  , _toText :: a -> Text
  , _fromText :: Text -> Either ParseError a
  }
makeLenses ''Expr

-- error -----------------------------------------------------------------------

-- | What can go wrong with (decoding) an 'Expr'
data ExprError = ExprError ExprType Text ParseError
makeClassyPrisms ''ExprError

instance Show ExprError where
  show (ExprError t s p) = strUnlines
    [ "Error trying to parse a <" <> unpack t <> "> from: " <> unpack s
    , show p ]

instance Exception ExprError
instance AsExprError SomeException where __ExprError = _SomeException

-- ops -------------------------------------------------------------------------

-- | Use an 'Expr' to render some value as 'Text'
encode :: Expr a -> a -> Text
encode = view toText

-- | Use an 'Expr' to attempt to decode some value from 'Text'
decode :: (AsExprError e, MonadError e m) => Expr a -> Text -> m a
decode e t = case e^.fromText $ t of
  Left err -> Err.throwing _ExprError (e^.exprType, t, err)
  Right a -> pure a

-- constructors ----------------------------------------------------------------

-- | Create an 'Expr' from an a list of named alternatives
namedExpr :: Eq a => ExprType -> Named a -> Expr a
namedExpr t ns = Expr t nameFor' (parse $ namedP ns)
  where
    nameFor' a = fromMaybe (error msg) $ nameFor a ns
    msg = devFail "Name left unspecified in Expr: <" <> unpack t <> ">"

-- | Create an 'Expr' from a printer and a parser.
customExpr :: ExprType -> (a -> Text) -> Parser a -> Expr a
customExpr t f p = Expr t f $ parse p

-- parsers ---------------------------------------------------------------------

-- | Parse the non-preamble portion of the string, any non-colon or double-colon
exprTail :: Parser Text
exprTail = do
  let nc = takeWhile1P (Just "non-colon character") (/= ':')
  let ec = ":" <$ string "::" <?> ":: (escaped colon)"
  mconcat <$> some (try ec <|> nc) <* (void eol <|> eof)
