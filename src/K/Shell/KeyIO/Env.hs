-- |

module K.Shell.KeyIO.Env where

import K.Shell.KeyIO.Initial
import K.Shell.KeyIO.LinEvdevSrc
import K.Shell.KeyIO.LinUinputSnk

-- env -------------------------------------------------------------------------

withKeySrc :: (UIO m, CanLog env m) => KeyInputCfg -> (KeySrc -> m a) -> m a
withKeySrc (LinEvdevSrc p) = withLinEvdevSrc p
withKeySrc _ = pure undefined

withKeySnk :: (UIO m, CanLog env m) => KeyOutputCfg -> (KeySnk -> m a) -> m a
withKeySnk (LinUinputSnk mn) = withLinUinputSnk mn
withKeySnk _ = pure undefined

withKeyRepeatEnv :: (UIO m, CanLog e m) => KeyRepeatCfg -> (KeyRepeatEnv -> m a) -> m a
withKeyRepeatEnv _ f = f KeyRepeatEnv

withKio :: (CanLog e m, UIO m, HasKioCfg c) => c -> (KioEnv -> m a) -> m a
withKio c f =
  withKeySrc (c^.keyInputCfg) $ \keysrc ->
    withKeySnk (c^.keyOutputCfg) $ \keysnk ->
      withKeyRepeatEnv (c^.keyRepeatCfg) $ \keyrep ->
        f $ KioEnv keysnk keysrc keyrep

type CanKio e m = (MonadReader e m, HasKioEnv e, MonadIO m)

waitEvent :: CanKio e m => m KioEvent
waitEvent = view (kioEnv . keySrc) >>= liftIO . srcEvent

sendEvent :: CanKio e m => KioEvent -> m ()
sendEvent e = view (kioEnv . keySnk) >>= \snk -> liftIO $ snkEvent snk e
