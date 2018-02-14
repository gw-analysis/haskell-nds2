{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.NDS2.Internals.Wrapper (Port, Connection, NDSError, connect, disconnect) where

#include <wrapper.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import System.IO.Unsafe
import Foreign.C
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import Control.Exception
import Data.Typeable
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as V
import Unsafe.Coerce

import Control.Lens
import Control.Arrow ((***))

{#context prefix = "hsnds2"#}

type Port    = {#type port_t#}
type GpsTime = {#type gps_time_t#}
type SizeT   = {#type size_t#}

-- | Structures
{#pointer *connection as Connection foreign finalizer destroy newtype#}
{#pointer *channel as ChannelsPtr foreign -> Channel #}

{#enum channel_type as ChannelType {underscoreToCase} deriving (Show, Eq) #}
{#enum data_type as DataType {underscoreToCase} deriving (Show, Eq) #}
{#enum protocol_type as ProtocolType {underscoreToCase} deriving (Show, Eq) #}

data Channel = Channel
  { _name       :: String
  , _type       :: ChannelType
  , _dataType   :: DataType
  , _sampleRate :: Float
  , _gain       :: Float
  , _slope      :: Float
  , _offset     :: Float
  } deriving (Show, Eq)

makeFields ''Channel

instance Storable Channel where
  sizeOf _ = {#sizeof channel #}
  alignment _ = {#alignof channel #}

  peek p = Channel
    <$> ({#get channel->name #} p >>= peekCString)
    <*> liftM (toEnum . fromIntegral) ({#get channel->type #} p)
    <*> liftM (toEnum . fromIntegral) ({#get channel->dataType #} p)
    <*> liftM realToFrac ({#get channel->sampleRate #} p)
    <*> liftM realToFrac ({#get channel->gain #} p)
    <*> liftM realToFrac ({#get channel->slope #} p)
    <*> liftM realToFrac ({#get channel->offset #} p)

  poke _ = undefined

-- | Exception
data NDSError = NDSError String deriving (Show, Typeable)
instance Exception NDSError

-- | Allocate a char* buffer of length ERRBUF_LENGTH + 1.
allocaErrBuf :: (CString -> IO a) -> IO a
allocaErrBuf f = allocaBytes ({#const ERRBUF_LENGTH#} + 1) $ \c_errbuf -> do
  pokeByteOff c_errbuf 0 '\0' -- Initialize the buffer with empty string
  f c_errbuf

-- | Check if error buffer is empty. If not, throw a NDSError.
checkErrBuf :: CString -> IO ()
checkErrBuf errbuf = do
  err <- peekCString errbuf
  when (length err > 0) $ throw (NDSError err)


-- | Use an array of String as argument, usually used to pass multiple names to
-- | C functions.
withStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withStringArray [] f = f nullPtr
withStringArray ss f = do
  ps <- mapM (\s -> withCString s return) ss
  withArray ps f


vmapCDoubleToDouble :: V.Vector CDouble -> V.Vector Double
-- In NHC, CDouble and Double are _not_ the same type.
#ifdef __NHC__
vmapCDoubleToDouble = V.map realToFrac
#else
vmapCDoubleToDouble = unsafeCoerce -- CAVEAT EMPTOR. Works for GHC.
#endif


-- peekCString :: CString -> IO String

-- Function declarations
{#fun unsafe connect {               `String'
                     , fromIntegral  `Port'
                     ,               `ProtocolType'
                     , allocaErrBuf- `String'       checkErrBuf*-
                     } -> `Connection' #}

{#fun unsafe disconnect { `Connection' } -> `()' #}

type ChannelNames = [String]

fetch :: Connection -> GpsTime -> GpsTime -> ChannelNames -> IO [V.Vector Double]
fetch conn startTime endTime channelList =
  withConnection conn $ \c_conn ->
  withStringArray channelList $ \c_channelList ->
  allocaArray nChannels $ \c_buffers {- double*[]; double* allocated by C -} ->
  allocaArray nChannels $ \c_bufferLengths {- size_t[] -} ->
  allocaErrBuf $ \c_errbuf -> do
    {#call unsafe fetch#} c_conn startTime endTime c_channelList (fromIntegral nChannels) c_buffers c_bufferLengths c_errbuf
    checkErrBuf c_errbuf

    buffers <- peekArray nChannels c_buffers
    bufferLengths <- peekArray nChannels c_bufferLengths

    forM (zip buffers bufferLengths) $ \(c_buf, bufLength) -> do
      bufPtr <- newForeignPtr hsnds2_free_buffer c_buf

      -- vmapCDoubleToDouble maps Vector CDouble to Vector Double.
      return . vmapCDoubleToDouble $ V.unsafeFromForeignPtr0 bufPtr (fromIntegral bufLength)

  where nChannels = length channelList

foreign import ccall unsafe "Wrapper.h &hsnds2_free_buffer"
  hsnds2_free_buffer :: FinalizerPtr CDouble


{#fun unsafe set_parameter as setParameter
  {               `Connection'
  ,               `String'     -- parameter
  ,               `String'     -- value
  , allocaErrBuf- `String' checkErrBuf*-
  } -> `Bool' #}

getParameter :: Connection -> String -> IO (Maybe String)
getParameter conn param = do
  fc_val <- getParameter_ conn param
  withForeignPtr fc_val $ \c_val ->
    if c_val == nullPtr
    then return Nothing
    else Just <$> peekCString c_val

type ForeignCString = ForeignPtr CChar

newForeignCString :: CString -> IO ForeignCString
newForeignCString = newForeignPtr c_free

{#fun unsafe get_parameter as getParameter_
  {               `Connection'
  ,               `String'      -- parameter
  , allocaErrBuf- `String' checkErrBuf*-
  } -> `ForeignCString' newForeignCString*  #}

type ChannelGlob = String

findChannels :: Connection -> ChannelGlob -> IO [Channel]
findChannels conn chanGlob = do
  (len, channelsPtr) <- findChannels_ conn chanGlob
  withForeignPtr channelsPtr $ peekArray len


peekChannels :: Ptr (Ptr Channel) -> IO (ChannelsPtr)
peekChannels ptr = peek ptr >>= newForeignPtr hsnds2_free_channels


-- findChannels_ :: Connection -> ChannelGlob -> IO (Int, Ptr Channel)
{#fun unsafe find_channels as findChannels_
  { `Connection'
  , `String'    -- channelGlob
  , alloca- `ChannelsPtr' peekChannels*
  , allocaErrBuf- `String' checkErrBuf*-
  } -> `Int' #}


foreign import ccall unsafe "wrapper.h &hsnds2_free_channels"
  hsnds2_free_channels :: FinalizerPtr Channel

foreign import ccall unsafe "stdlib.h &free"
  c_free :: FinalizerPtr a

--------------------------------------------------------------------------
{- Test Commands:
conn <- connect "10.68.10.122" 8088 ProtocolTry
let chanList = ["K1:PEM-TEMPERATURE_RACK_IMC", "K1:PEM-HUMIDITY_RACK_IMC"]
setParameter conn "GAP_HANDLER" "STATIC_HANDLER_NAN"
findChannels conn "*CRY-TEMPERATURE*"
res <- fetch conn 1202078040 1202078160 chanList
-}
