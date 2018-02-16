import Network.NDS2
import Data.Default
import Control.Lens
import Control.Monad

main :: IO ()
main = do
  conn <- connect $ def & hostname .~ "10.68.10.122"
                        & port     .~ 8088

  (Just p) <- getParameter conn "GAP_HANDLER"
  putStrLn p

  setParameter conn "GAP_HANDLER" "STATIC_HANDLER_NAN"
  (Just p) <- getParameter conn "GAP_HANDLER"
  putStrLn p

  let chanList = ["K1:PEM-TEMPERATURE_RACK_IMC", "K1:PEM-HUMIDITY_RACK_IMC"]
      nChannels = length chanList
  res <- fetch conn $ def & startGpsTime .~ 1202078040
                          & endGpsTime   .~ 1202078160
                          & channelNames .~ chanList


  print res

  initStream conn $ def & channelNames .~ chanList

  forever $ recvNext conn nChannels >>= print
