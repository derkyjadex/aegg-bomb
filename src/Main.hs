module Main (main) where

import           Control.Concurrent
import           Game
import           Graphics
import           Physics
import           Server

level :: [Box]
level =
  [((-11,-11),(-10,10))
  ,((-11,10),(10,11))
  ,((10,-10),(11,11))
  ,((-10,-11),(11,-10))
  ,((-6,-6),(-5,6))]

main :: IO ()
main =
  do gameChan <- newGameChan
     renderChan <- newRenderChan
     let game = newGame level
     _ <- forkIO $ runGame gameChan renderChan game
     _ <- forkIO $ runServer gameChan
     renderMain renderChan
