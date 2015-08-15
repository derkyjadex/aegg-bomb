import Control.Concurrent
import Game
import Graphics
import Server

main :: IO ()
main = do
  gameChan <- newGameChan
  renderChan <- newRenderChan
  forkIO $ runGame gameChan renderChan newGame
  forkIO $ runServer gameChan
  renderMain renderChan
