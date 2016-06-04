module DB (
  withDB
) where

import Control.Monad.Trans
import Data.Pool
import Database.PostgreSQL.Simple

-- Pick a connection handle from the pool and use it to
-- execute the given request.
-- The MonadIO bit lets me avoid caring about where this will run,
-- although we're always using it in the `ActionT SproxyError IO` monad
-- but the more general type signature keeps us from doing anything irrelevant
withDB :: MonadIO m 
       => Pool Connection
       -> (Connection -> IO a)
       -> m a
withDB pool act = liftIO $ withResource pool (liftIO . act)
