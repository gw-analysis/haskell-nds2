{-# LANGUAGE ForeignFunctionInterface #-}

module Network.NDS2.Internals.Wrapper
  (Connection
  , connect
  , disconnect
  , fetch
  , setParameter
  , getParameter
  , findChannels
  , startRealtime
  , next) where

#include <wrapper.h>

import Control.Applicative ((<$>), (<*>))
import Control.Exception
import Control.Monad
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import qualified Data.Vector.Storable as Vec
import Unsafe.Coerce

import Network.NDS2.Internals.Types


{#context prefix = "hsnds2"#}

type CPort          = {#type port_t          #}
type CGpsSecond     = {#type gps_second_t    #}
type CGpsNanosecond = {#type gps_nanosecond_t#}
type CSizeT         = {#type size_t          #}
type CStride        = CGpsSecond


type ForeignCString = ForeignPtr CChar

{#pointer *connection_t as Connection foreign finalizer destroy newtype#}
{#pointer *channel_t as ChannelsPtr foreign -> Channel #}
{#pointer *out_buffer_t as BufferPtr -> Buffer #}

-------------------------------------------------------------------------------
-- Utility functions

-- |Allocate a char* buffer of length ERRBUF_LENGTH + 1.
allocaErrBuf :: (CString -> IO a) -> IO a
allocaErrBuf f = allocaBytes ({#const ERRBUF_LENGTH#} + 1) $ \c_errbuf -> do
  pokeByteOff c_errbuf 0 '\0' -- Initialize the buffer with empty string
  f c_errbuf

-- |Check if error buffer is empty. If not, throw a NDSError.
checkErrBuf :: CString -> IO ()
checkErrBuf errbuf = do
  err <- peekCString errbuf
  when (length err > 0) $ throw (NDSError err)


-- |Use an array of String as argument, usually used to pass multiple names to
-- C functions.
withStringArray :: [String] -> (Ptr CString -> IO a) -> IO a
withStringArray [] f = f nullPtr
withStringArray ss f = do
  ps <- mapM (\s -> withCString s return) ss
  withArray ps f


-- |Create a ForeignPtr of a Ptr allocated by C malloc.
newForeignCPtr :: Ptr a -> IO (ForeignPtr a)
newForeignCPtr = newForeignPtr c_free

foreign import ccall unsafe "stdlib.h &free"
  c_free :: FinalizerPtr a

-- |Peek channel_t**, where the channel list is allocated by C malloc.
peekChannels :: Ptr (Ptr Channel) -> IO (ChannelsPtr)
peekChannels ptr = peek ptr >>= newForeignPtr hsnds2_free_channels

foreign import ccall unsafe "wrapper.h &hsnds2_free_channels"
  hsnds2_free_channels :: FinalizerPtr Channel

-- |Convert an enum to CInt.
enumToCInt :: (Enum a) => a -> CInt
enumToCInt = fromIntegral . fromEnum

-- |Allocate an out_buffer_t* buffer with n elements.
allocaBuffers :: Int -> (Ptr Buffer -> IO a) -> IO a
allocaBuffers n = allocaBytes (n * {#sizeof out_buffer_t#})

-- |Peek an out_buffer_t* struct, where some elements in the struct
--  are allocated by C malloc.
peekBuffer :: Ptr Buffer -> IO Buffer
peekBuffer p = Buffer
  <$> peekChannel p      -- Allocated by C
  <*> liftM fromIntegral ({#get out_buffer_t->startGpsSecond     #} p)
  <*> liftM fromIntegral ({#get out_buffer_t->startGpsNanosecond #} p)
  <*> liftM fromIntegral ({#get out_buffer_t->stopGpsSecond      #} p)
  <*> peekTimeSeries p   -- Allocated by C

  where
    -- Peek the channel in out_buffer_t, which is allocated by C malloc.
    peekChannel p = do
      chanPtr <- ({#get out_buffer_t->channelInfo #} p)
                 >>= newForeignPtr hsnds2_free_channel
      withForeignPtr chanPtr peek

    -- Peek the timeseries in out_buffer_t, which is allocated by C malloc.
    peekTimeSeries p = do
      len    <- liftM fromIntegral ({#get out_buffer_t->timeseries_length #} p)
      bufPtr <- ({#get out_buffer_t->timeseries #} p)
                >>= newForeignPtr hsnds2_free_timeseries
      return . vmapCDoubleToDouble $ Vec.unsafeFromForeignPtr0 bufPtr len

-- |Peek a list of Buffers with n elements.
peekBuffers :: Int -> Ptr Buffer -> IO [Buffer]
peekBuffers n bufBegin = go [] $ bufBegin `plusPtr` (sizeofBuf * (n-1))
  where
    -- Peek each Buffer element by element, in reverse order.
    go l p
      | p < bufBegin = return l
      | otherwise    = do
          b <- peekBuffer p
          go (b:l) $ p `plusPtr` (-sizeofBuf)

    sizeofBuf = {#sizeof out_buffer_t #}


-- |Convert a CDouble Vector to a Double Vector.
vmapCDoubleToDouble :: Vec.Vector CDouble -> Vec.Vector Double
-- In NHC, CDouble and Double are _not_ the same type.
#ifdef __NHC__
vmapCDoubleToDouble = Vec.map realToFrac
#else
vmapCDoubleToDouble = unsafeCoerce -- CAVEAT EMPTOR. Works for GHC.
#endif


foreign import ccall unsafe "Wrapper.h &hsnds2_free_channel"
  hsnds2_free_channel :: FinalizerPtr Channel

foreign import ccall unsafe "Wrapper.h &hsnds2_free_timeseries"
  hsnds2_free_timeseries :: FinalizerPtr CDouble

-------------------------------------------------------------------------------

{#fun unsafe connect {               `String'
                     , fromIntegral  `CPort'
                     , enumToCInt    `ProtocolType'
                     , allocaErrBuf- `String'       checkErrBuf*-
                     } -> `Connection' #}

{#fun unsafe disconnect { `Connection' } -> `()' #}


fetch :: Connection      -- ^ The NDS Connection handle
      -> CGpsSecond      -- ^ Start GPS time in seconds
      -> CGpsSecond      -- ^ Stop GPS time in seconds
      -> [String]        -- ^ List of channel names
      -> IO [Buffer]     -- ^ List of out buffers
fetch conn startTime stopTime channelNames =
  withConnection conn $ \c_conn ->
  withStringArray channelNames $ \c_channelNames ->
  allocaBuffers nChannels $ \c_buffers {- out_buffer_t[] -} ->
  allocaErrBuf $ \c_errbuf -> do
    {#call unsafe fetch#} c_conn
                          startTime
                          stopTime
                          c_channelNames
                          (fromIntegral nChannels)
                          c_buffers
                          c_errbuf
    checkErrBuf c_errbuf

    peekBuffers nChannels c_buffers

  where nChannels = length channelNames


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


{#fun unsafe get_parameter as getParameter_
  {               `Connection'
  ,               `String'      -- parameter
  , allocaErrBuf- `String' checkErrBuf*-
  } -> `ForeignCString' newForeignCPtr*  #}


findChannels :: Connection -> String -> IO [Channel]
findChannels conn chanGlob = do
  (len, channelsPtr) <- findChannels_ conn chanGlob
  withForeignPtr channelsPtr $ peekArray len


-- findChannels_ :: Connection -> ChannelGlob -> IO (Int, Ptr Channel)
{#fun unsafe find_channels as findChannels_
  { `Connection'
  , `String'    -- channelGlob
  , alloca- `ChannelsPtr' peekChannels*
  , allocaErrBuf- `String' checkErrBuf*-
  } -> `Int' #}


startRealtime :: Connection -> [String] -> CStride -> IO ()
startRealtime conn channelNames stride =
  withConnection conn $ \c_conn ->
  withStringArray channelNames $ \c_channelNames ->
  allocaErrBuf $ \c_errbuf -> do
    {#call unsafe start_realtime#} c_conn
                                   c_channelNames
                                   (fromIntegral nChannels)
                                   stride
                                   c_errbuf
    checkErrBuf c_errbuf

  where nChannels = length channelNames

next :: Connection -> Int -> IO (Maybe [Buffer])
next conn nChannels =
  withConnection conn $ \c_conn ->
  allocaBuffers nChannels $ \c_buffers {- out_buffer_t[] -} ->
  allocaErrBuf $ \c_errbuf -> do
    ret <- {#call unsafe next#} c_conn
                                (fromIntegral nChannels)
                                c_buffers
                                c_errbuf
    checkErrBuf c_errbuf

    if Errno (-ret) == eRANGE -- Out of range (iteration has finished).
    then return Nothing
    else Just <$> peekBuffers nChannels c_buffers
