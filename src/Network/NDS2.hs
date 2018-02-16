{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Network.NDS2 where

import           Network.NDS2.Types

import           Data.Default
import           GHC.Generics
import qualified Network.NDS2.Internals.Wrapper as I
import           Network.NDS2.Types

import           Control.Lens

--------------------------------------------------------------------------------

data ConnectParams = ConnectParams
 { _connectParamsHostname     :: String       -- ^ Hostname
 , _connectParamsPort         :: Port         -- ^ TCP port
 , _connectParamsProtocolType :: ProtocolType -- ^ Protocol type. Defaults to ProtocolTry.
 } deriving (Eq, Show, Generic, Default)
makeFields ''ConnectParams

data FetchParams = FetchParams
  { _fetchParamsStartGpsTime :: GpsSecond
  , _fetchParamsStopGpsTime  :: GpsSecond
  , _fetchParamsChannelNames :: ChannelNames
  } deriving (Eq, Show, Generic, Default)
makeFields ''FetchParams

data StreamParams = StartRealtimeParams
  { _streamParamsChannelNames :: ChannelNames
  , _streamParamsStride       :: Stride
  } deriving (Eq, Show, Generic)
makeFields ''StreamParams

instance Default StreamParams where
  def = StartRealtimeParams [] 0

--------------------------------------------------------------------------------



-- | Create a connection to a NDS server.
connect :: ConnectParams -> IO Connection
connect params = I.connect (params^.hostname)
                           (fromIntegral $ params^.port)
                           (params^.protocolType)

-- | Disconnect from a NDS server.
disconnect :: Connection -> IO ()
disconnect = I.disconnect

-- | Get the current parameter value for a given parameter.
getParameter :: Connection -> String -> IO (Maybe String)
getParameter = I.getParameter

-- | Set a given parameter to value given.
setParameter :: Connection
             -> String     -- ^ Parameter name
             -> String     -- ^ New parameter value
             -> IO Bool    -- ^ A bool indicating success or failure
setParameter = I.setParameter

-- | Find channels matching the globbing pattern given.
findChannels :: Connection -> ChannelGlob -> IO [Channel]
findChannels = I.findChannels

-- | Fetch data from the server.
fetch :: Connection -> FetchParams -> IO [Buffer]
fetch conn params = I.fetch conn
                            (fromIntegral $ params^.startGpsTime)
                            (fromIntegral $ params^.stopGpsTime)
                            (params^.channelNames)


-- | Start a live data stream.
initStream :: Connection -> StreamParams  -> IO ()
initStream conn params = I.startRealtime conn
                                         (params^.channelNames)
                                         (fromIntegral $ params^.stride)

-- | Read the next data block from the live data stream.
recvNext :: Connection
         -> Int                     -- ^ Number of channels
         -> IO (Maybe [Buffer]) -- ^ A list of buffers, or Nothing if the data stream has finished
recvNext = I.next
