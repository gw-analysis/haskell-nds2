{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Network.NDS2.Internals.Types where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Exception
import Data.Default
import Data.Typeable
import Data.Vector.Storable (Vector)
import Foreign.Storable
import Foreign.C.String

#include <wrapper.h>

{#context prefix = "hsnds2"#}

type Port          = Int
type GpsSecond     = Int
type GpsNanosecond = Int
type ChannelGlob   = String
type Stride        = GpsSecond
type ChannelNames  = [String]

type TimeSeries    = Vector Double


{#enum channel_type as ChannelType {underscoreToCase} deriving (Show, Eq) #}

{#enum data_type as DataType {underscoreToCase} deriving (Show, Eq) #}

{#enum protocol_type as ProtocolType {underscoreToCase} deriving (Show, Eq) #}

instance Default ProtocolType where
  def = ProtocolTry

-- | Corresponds to channel_t struct.
data Channel = Channel
  { _channelName       :: !String
  , _channelChannelType:: !ChannelType
  , _channelDataType   :: !DataType
  , _channelSampleRate :: !Float
  , _channelGain       :: !Float
  , _channelSlope      :: !Float
  , _channelOffset     :: !Float
  } deriving (Show, Eq)

makeFields ''Channel

instance Storable Channel where
  sizeOf _    = {#sizeof  channel_t #}
  alignment _ = {#alignof channel_t #}

  peek p = Channel
    <$> ({#get channel_t->name #} p >>= peekCString)
    <*> liftM (toEnum . fromIntegral) ({#get channel_t->type       #} p)
    <*> liftM (toEnum . fromIntegral) ({#get channel_t->dataType   #} p)
    <*> liftM realToFrac              ({#get channel_t->sampleRate #} p)
    <*> liftM realToFrac              ({#get channel_t->gain       #} p)
    <*> liftM realToFrac              ({#get channel_t->slope      #} p)
    <*> liftM realToFrac              ({#get channel_t->offset     #} p)

  poke _ = undefined

-- | Corresponds to out_buffer_t struct.
data Buffer = Buffer
 { _bufferChannelInfo        :: !Channel
 , _bufferStartGpsSecond     :: !GpsSecond
 , _bufferStartGpsNanosecond :: !GpsNanosecond
 , _bufferStopGpsTime        :: !GpsSecond
 , _bufferTimeSeries         :: !TimeSeries
 } deriving (Show, Eq)

makeFields ''Buffer


data NDSError = NDSError String deriving (Show, Typeable)
instance Exception NDSError
