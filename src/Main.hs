import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Function (fix)
import Physics
import Graphics
import Game
import Server

main :: IO ()
main = do
  chan <- newRenderChan
  forkIO $ runServer chan
  renderMain chan
