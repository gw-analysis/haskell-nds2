{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.NDS2.Internals.Wrapper (Port, Connection, NDSError, connect, disconnect) where

#include <wrapper.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import System.IO.Unsafe
import Foreign.C
import Control.Monad
import Control.Applicative ((<$>))
import Control.Exception
import Data.Typeable

{#context prefix = "hsnds2_" add prefix = "cc_" #}

{#enum channel_type as ChannelType {underscoreToCase} deriving (Show, Eq) #}
{#enum data_type as DataType {underscoreToCase} deriving (Show, Eq) #}
{#enum protocol_type as ProtocolType {underscoreToCase} deriving (Show, Eq) #}

type Port = Int

-- Structures
{#pointer *Connection as Connection foreign finalizer destroy newtype#}


-- Exception
data NDSError = NDSError String deriving (Show, Typeable)
instance Exception NDSError

allocaErrbuf = allocaBytes ({#const ERRBUF_LENGTH#} + 1)

checkErrbuf :: CString -> IO ()
checkErrbuf errbuf = do
  err <- peekCString errbuf
  when (length err > 0) $ throw (NDSError err)


-- peekCString :: CString -> IO String

-- Function declarations
{#fun unsafe connect { `String'
                     , fromIntegral `Port'
                     , `ProtocolType'
                     , allocaErrbuf- `String' checkErrbuf*-
                     } -> `Connection' #}

{#fun unsafe disconnect { `Connection' } -> `()' #}
