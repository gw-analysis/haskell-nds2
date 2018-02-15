{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Network.NDS2 where

import Network.NDS2.Types

import           Data.Default
import           GHC.Generics
import Network.NDS2.Types
import qualified Network.NDS2.Internals.Wrapper as I

import           Control.Lens

data ConnectParams = ConnectParams
 { _connectParamsHostname     :: String -- ^ Hostname
 , _connectParamsPort         :: Port   -- ^ TCP port
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
  , _startRealtimeParamsStride :: Stride
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

getParameter :: Connection -> String -> IO (Maybe String)
getParameter = I.getParameter

setParameter :: Connection -> String -> String -> IO Bool
setParameter = I.setParameter

findChannels :: Connection -> ChannelGlob -> IO [Channel]
findChannels = I.findChannels

fetch :: Connection -> FetchParams -> IO [DataVector]
fetch conn params = I.fetch conn
                            (fromIntegral $ params^.startGpsTime)
                            (fromIntegral $ params^.endGpsTime)
                            (params^.channelNames)


startRealtime :: Connection -> StartRealtimeParams  -> IO ()
startRealtime conn params = I.startRealtime conn
                                            (params^.channelNames)
                                            (fromIntegral $ params^.stride)

next :: Connection -> Int -> IO (Maybe [DataVector])
next = I.next
