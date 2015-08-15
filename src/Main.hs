import Control.Concurrent
import Graphics
import Game
import Server

main :: IO ()
main = do
  chan <- newRenderChan
  forkIO $ runServer chan
  renderMain chan
