import Network.NDS2
import Network.NDS2.Conduit
import Data.Default
import Control.Lens
import Control.Monad
import System.Environment
import Data.Conduit hiding (connect)
import qualified Data.Conduit.List as CL
import qualified Data.Vector.Storable as V

main :: IO ()
main = do
  args <- getArgs
  let isLocal = length args == 1 && head args == "local"
      connParams | isLocal   = def & hostname .~ "127.0.0.1"    & port .~ 32000
                 | otherwise = def & hostname .~ "10.68.10.122" & port .~ 8088
      chanList   | isLocal   = ["X1:PEM-1", "X1:PEM-2"]
                 | otherwise = ["K1:PEM-TEMPERATURE_RACK_IMC", "K1:PEM-HUMIDITY_RACK_IMC"]
      nChannels = length chanList

  conn <- connect connParams

  setParameter conn "GAP_HANDLER" "STATIC_HANDLER_NAN"

  runConduit $ (ndsSource' conn (def & channelNames .~ chanList)) .| sink
  where
    sink = CL.mapM_ print
