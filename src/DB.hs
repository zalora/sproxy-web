module DB (createDBPool, withDB, DBPool, Connection) where

import Config
import Control.Monad.Trans
import Data.Pool
import Database.PostgreSQL.Simple

type DBPool = Pool Connection

-- Initialize our connection pool
createDBPool :: IO DBPool
createDBPool =
    createPool (connectPostgreSQL dbConnectionString)
               close
               1   -- stripe count
               100 -- amount of secs it stays alive
               50  -- at most 50 concurrent connections

-- Pick a connection handle from the pool and use it to
-- execute the given request.
-- The MonadIO bit lets me avoid caring about where this will run,
-- although we're always using it in the `ActionT SproxyError IO` monad
-- but the more general type signature keeps us from doing anything irrelevant
withDB :: MonadIO m 
       => DBPool 
       -> (Connection -> IO a)
       -> m a
withDB pool act = liftIO $ withResource pool (liftIO . act)
