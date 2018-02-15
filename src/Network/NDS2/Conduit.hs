module Network.NDS2.Conduit where

import Network.NDS2
import Network.NDS2.Types
import Data.Conduit (Source, yield)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Lens

ndsSource' :: Connection -> StreamParams -> Source IO [DataVector]
ndsSource' conn params = do
  liftIO $ initStream conn params
  forever $ do
    res <- liftIO $ recvNext conn nChannels
    case res of
      Nothing       -> return ()
      Just dataVecs -> yield dataVecs

  where
    nChannels = length $ params^.channelNames

ndsSource :: ConnectParams -> StreamParams -> Source IO [DataVector]
ndsSource connParams streamParams = do
  conn <- liftIO $ connect connParams
  liftIO $ setParameter conn "GAP_HANDLER" "STATIC_HANDLER_NAN"
  ndsSource' conn streamParams
