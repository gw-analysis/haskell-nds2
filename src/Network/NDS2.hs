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

data ConnectParams = ConnectParams
 { _connectParamsHostname     :: String       -- ^ Hostname
 , _connectParamsPort         :: Port         -- ^ TCP port
 , _connectParamsProtocolType :: ProtocolType -- ^ Protocol type. Defaults to ProtocolTry.
 } deriving (Eq, Show, Generic, Default)

makeFields ''ConnectParams

data FetchParams = FetchParams
  { _fetchParamsStartGpsTime :: GpsTime
  , _fetchParamsEndGpsTime   :: GpsTime
  , _fetchParamsChannelNames :: ChannelNames
  } deriving (Eq, Show, Generic, Default)
makeFields ''FetchParams

data StartRealtimeParams = StartRealtimeParams
  { _startRealtimeParamsChannelNames :: ChannelNames
  , _startRealtimeParamsStride       :: Stride
  } deriving (Eq, Show, Generic)
makeFields ''StartRealtimeParams

instance Default StartRealtimeParams where
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
fetch :: Connection -> FetchParams -> IO [DataVector]
fetch conn params = I.fetch conn
                            (fromIntegral $ params^.startGpsTime)
                            (fromIntegral $ params^.endGpsTime)
                            (params^.channelNames)


-- | Start a realtime data stream.
startRealtime :: Connection -> StartRealtimeParams  -> IO ()
startRealtime conn params = I.startRealtime conn
                                            (params^.channelNames)
                                            (fromIntegral $ params^.stride)

-- | Read the next data block from the realtime data stream.
next :: Connection
     -> Int                     -- ^ Number of channels
     -> IO (Maybe [DataVector]) -- ^ A list of DataVectors, or Nothing if the data stream has finished
next = I.next
