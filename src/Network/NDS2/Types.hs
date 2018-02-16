module Network.NDS2.Types
  ( Channel(..)
  , Buffer(..)
  , ChannelType(..)
  , DataType(..)
  , ProtocolType(..)
  , NDSError(..)
  , Connection
  , Port
  , GpsSecond
  , ChannelGlob
  , Stride
  , ChannelNames
  , TimeSeries
  ) where

import           Data.Typeable

import           Network.NDS2.Internals.Types
import           Network.NDS2.Internals.Wrapper (Connection)
