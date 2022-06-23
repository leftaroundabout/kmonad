-- |

module K.Shell.Cfg.KbdFile where

import K.Shell.Cfg.Initial

data KbdFile = KbdFile

loadKbdFile :: (MonadIO m, MonadReader e m, HasLocaleCfg e, HasRunCfg e) => m KbdFile
loadKbdFile = pure KbdFile
