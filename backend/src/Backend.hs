{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where


import Common.Route
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Network.WebSockets.Snap
import Obelisk.Backend
import Control.Monad.IO.Class

import qualified Server

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      backendState <- liftIO Server.initBackend
      serve $ \case
        BackendRoute_Missing :=> Identity () -> return ()
        BackendRoute_WebSocket :=> Identity () ->
          runWebSocketsSnap (Server.application backendState)

  , _backend_routeEncoder = backendRouteEncoder
  }

