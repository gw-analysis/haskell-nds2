{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.NDS2.Internals.Wrapper (Port, Connection, NDSError, connect, disconnect) where

#include <wrapper.h>

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.C.Error(Errno, eRANGE)
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
  { _name       :: !String
  , _type       :: !ChannelType
  , _dataType   :: !DataType
  , _sampleRate :: !Float
  , _gain       :: !Float
  , _slope      :: !Float
  , _offset     :: !Float
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

-- | Create a ForeignPtr of a Ptr allocated by C malloc.
newForeignCPtr :: Ptr a -> IO (ForeignPtr a)
newForeignCPtr = newForeignPtr c_free

foreign import ccall unsafe "stdlib.h &free"
  c_free :: FinalizerPtr a

copyBuffers :: Int -> Ptr (Ptr CDouble) -> Ptr SizeT -> IO [V.Vector Double]
copyBuffers nChannels c_buffers c_bufferLengths = do
  buffers <- peekArray nChannels c_buffers
  bufferLengths <- peekArray nChannels c_bufferLengths

  forM (zip buffers bufferLengths) $ \(c_buf, bufLength) -> do
    bufPtr <- newForeignPtr hsnds2_free_buffer c_buf

    -- vmapCDoubleToDouble maps Vector CDouble to Vector Double.
    return . vmapCDoubleToDouble $ V.unsafeFromForeignPtr0 bufPtr (fromIntegral bufLength)


-- Function declarations
{#fun unsafe connect {               `String'
                     , fromIntegral  `Port'
                     ,               `ProtocolType'
                     , allocaErrBuf- `String'       checkErrBuf*-
                     } -> `Connection' #}

{#fun unsafe disconnect { `Connection' } -> `()' #}

type ChannelNames = [String]

fetch :: Connection -> GpsTime -> GpsTime -> ChannelNames -> IO [V.Vector Double]
fetch conn startTime endTime channelNames =
  withConnection conn $ \c_conn ->
  withStringArray channelNames $ \c_channelNames ->
  allocaArray nChannels $ \c_buffers {- double*[]; double* allocated by C -} ->
  allocaArray nChannels $ \c_bufferLengths {- size_t[] -} ->
  allocaErrBuf $ \c_errbuf -> do
    {#call unsafe fetch#} c_conn startTime endTime c_channelNames (fromIntegral nChannels) c_buffers c_bufferLengths c_errbuf
    checkErrBuf c_errbuf

    copyBuffers nChannels c_buffers c_bufferLengths

  where nChannels = length channelNames

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

{#fun unsafe get_parameter as getParameter_
  {               `Connection'
  ,               `String'      -- parameter
  , allocaErrBuf- `String' checkErrBuf*-
  } -> `ForeignCString' newForeignCPtr*  #}

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


type Stride = GpsTime

startRealtime :: Connection -> ChannelNames -> Stride -> IO ()
startRealtime conn channelNames stride =
  withConnection conn $ \c_conn ->
  withStringArray channelNames $ \c_channelNames ->
  allocaErrBuf $ \c_errbuf -> do
    {#call unsafe start_realtime#} c_conn c_channelNames (fromIntegral nChannels) stride c_errbuf
    checkErrBuf c_errbuf

  where nChannels = length channelNames

next :: Connection -> Int -> IO (Maybe [V.Vector Double])
next conn nChannels =
  withConnection conn $ \c_conn ->
  allocaArray nChannels $ \c_buffers {- double*[]; double* allocated by C -} ->
  allocaArray nChannels $ \c_bufferLengths {- size_t[] -} ->
  allocaErrBuf $ \c_errbuf -> do
    ret <- {#call unsafe next#} c_conn c_buffers c_bufferLengths (fromIntegral nChannels) c_errbuf
    checkErrBuf c_errbuf

    if Errno (-ret) == eRANGE -- Out of range (iteration has finished).
    then return Nothing
    else Just <$> copyBuffers nChannels c_buffers c_bufferLengths


--------------------------------------------------------------------------
{- Test Commands:
conn <- connect "10.68.10.122" 8088 ProtocolTry
let chanList = ["K1:PEM-TEMPERATURE_RACK_IMC", "K1:PEM-HUMIDITY_RACK_IMC"]
setParameter conn "GAP_HANDLER" "STATIC_HANDLER_NAN"
findChannels conn "*CRY-TEMPERATURE*"
res <- fetch conn 1202078040 1202078160 chanList
-}
