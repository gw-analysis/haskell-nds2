module Network.NDS2.Conduit where

import Network.NDS2
import Network.NDS2.Types
import Data.Conduit (Source, yield)
import Control.Monad (forever)
import Control.Monad.Trans
import Control.Lens

-- | Conduit source that streams live data from a NDS server.
ndsSource :: ConnectParams     -- ^ Connect parameters.
          -> StreamParams      -- ^ Stream parameters.
          -> Source IO [Buffer]
ndsSource connParams streamParams = do
  conn <- liftIO $ connect connParams
  liftIO $ setParameter conn "GAP_HANDLER" "STATIC_HANDLER_NAN"
  ndsSource' conn streamParams

-- | Conduit source that streams live data from a NDS server.
-- In this variant, the connection is assumed to be pre-established.
ndsSource' :: Connection         -- ^ The connection handle.
           -> StreamParams       -- ^ Stream parameters.
           -> Source IO [Buffer]
ndsSource' conn params = do
  liftIO $ initStream conn params
  forever $ do
    res <- liftIO $ recvNext conn nChannels
    case res of
      Nothing       -> return ()
      Just dataVecs -> yield dataVecs

  where
    nChannels = length $ params^.channelNames
