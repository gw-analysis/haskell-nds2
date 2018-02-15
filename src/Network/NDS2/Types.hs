module Network.NDS2.Types
  ( Channel(..)
  , ChannelType(..)
  , DataType(..)
  , ProtocolType(..)
  , NDSError(..)
  , Connection
  , Port
  , GpsTime
  , ChannelGlob
  , Stride
  , ChannelNames
  , DataVector
  ) where

import           Data.Typeable

import           Data.Vector.Storable           (Vector)
import           Network.NDS2.Internals.Types   (Channel, ChannelType, DataType,
                                                 NDSError, ProtocolType)
import           Network.NDS2.Internals.Wrapper (Connection)


type Port         = Int
type GpsTime      = Int
type ChannelGlob  = String
type Stride       = GpsTime
type ChannelNames = [String]

type DataVector   = Vector Double
