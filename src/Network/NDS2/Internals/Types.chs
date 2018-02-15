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
import Foreign.Storable
import Foreign.C.String

#include <wrapper.h>

{#context prefix = "hsnds2"#}


{#enum channel_type as ChannelType {underscoreToCase} deriving (Show, Eq) #}

{#enum data_type as DataType {underscoreToCase} deriving (Show, Eq) #}

{#enum protocol_type as ProtocolType {underscoreToCase} deriving (Show, Eq) #}

instance Default ProtocolType where
  def = ProtocolTry

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
  sizeOf _    = {#sizeof channel #}
  alignment _ = {#alignof channel #}

  peek p = Channel
    <$> ({#get channel->name #} p >>= peekCString)
    <*> liftM (toEnum . fromIntegral) ({#get channel->type       #} p)
    <*> liftM (toEnum . fromIntegral) ({#get channel->dataType   #} p)
    <*> liftM realToFrac              ({#get channel->sampleRate #} p)
    <*> liftM realToFrac              ({#get channel->gain       #} p)
    <*> liftM realToFrac              ({#get channel->slope      #} p)
    <*> liftM realToFrac              ({#get channel->offset     #} p)

  poke _ = undefined


data NDSError = NDSError String deriving (Show, Typeable)
instance Exception NDSError
