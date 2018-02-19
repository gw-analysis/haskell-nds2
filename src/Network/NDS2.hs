{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Network.NDS2
  ( module Network.NDS2
  , module Network.NDS2.Types
  ) where

import           Control.Lens
import           Data.Default
import           GHC.Generics
import qualified Network.NDS2.Internals.Wrapper as I
import           Network.NDS2.Types

--------------------------------------------------------------------------------

data ConnectParams = ConnectParams
 { _connectParamsHostname     :: String       -- ^ Hostname.
 , _connectParamsPort         :: Port         -- ^ TCP port.
 , _connectParamsProtocolType :: ProtocolType -- ^ Protocol type. Defaults to ProtocolTry.
 } deriving (Eq, Show, Generic)

makeFields ''ConnectParams

instance Default ConnectParams where
  def = ConnectParams "" 0 ProtocolTry


data FetchParams = FetchParams
  { _fetchParamsStartGpsTime :: GpsSecond     -- ^ Start GPS time in seconds.
  , _fetchParamsStopGpsTime  :: GpsSecond     -- ^ Stop GPS time in seconds.
  , _fetchParamsChannelNames :: ChannelNames  -- ^ A list of channel names.
  } deriving (Eq, Show, Generic)

makeFields ''FetchParams

instance Default FetchParams where
  def = FetchParams 0 0 []


data StreamParams = StreamParams
  { _streamParamsChannelNames :: ChannelNames -- ^ A list of channel names.
  , _streamParamsStride       :: Stride       -- ^ Stride.
  } deriving (Eq, Show, Generic)

makeFields ''StreamParams

instance Default StreamParams where
  def = StreamParams [] 0

--------------------------------------------------------------------------------



-- | Create a connection to a NDS server.
connect :: ConnectParams  -- ^ Connect parameters.
        -> IO Connection  -- ^ A new connection handle.
connect params = I.connect (params^.hostname)
                           (fromIntegral $ params^.port)
                           (params^.protocolType)

-- | Disconnect from a NDS server.
disconnect :: Connection  -- ^ The connection handle.
           -> IO ()
disconnect = I.disconnect

-- | Get the current parameter value for a given parameter.
getParameter :: Connection        -- ^ The connection handle.
             -> String            -- ^ Parameter name to request.
             -> IO (Maybe String) -- ^ Parameter value; Nothing if it does not exist.
getParameter = I.getParameter

-- | Set a given parameter to value given.
setParameter :: Connection -- ^ The connection handle.
             -> String     -- ^ Name of the parameter to set.
             -> String     -- ^ New parameter value.
             -> IO Bool    -- ^ Bool indicating success or failure.
setParameter = I.setParameter

-- | Find channels matching the globbing pattern given.
findChannels :: Connection   -- ^ The connection handle.
             -> ChannelGlob  -- ^ The channel glob, a string that may contain wildcards.
             -> IO [Channel] -- ^ A list of channels matching the channel glob.
findChannels = I.findChannels

-- | Fetch data from the server.
fetch :: Connection   -- ^ The connection handle.
      -> FetchParams  -- ^ Fetch parameters.
      -> IO [Buffer]  -- ^ A list of buffers returned from server.
fetch conn params = I.fetch conn
                            (fromIntegral $ params^.startGpsTime)
                            (fromIntegral $ params^.stopGpsTime)
                            (params^.channelNames)


-- | Start a live data stream.
--
-- Note that it is not possible to stop the stream after this function is
-- called. A new connection must be created to request other data.
initStream :: Connection   -- ^ The connection handle.
           -> StreamParams -- ^ Stream parameters.
           -> IO ()
initStream conn params = I.startRealtime conn
                                         (params^.channelNames)
                                         (fromIntegral $ params^.stride)

-- | Read the next data block from the live data stream.
recvNext :: Connection          -- ^ The connection handle.
         -> Int                 -- ^ The number of channels. This must match the
                                -- length of channelNames given to initStream.
         -> IO (Maybe [Buffer]) -- ^ A list of buffers, or Nothing if
                                -- the data stream has finished.
recvNext = I.next
