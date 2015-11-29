module Main (main) where

import           Control.Concurrent
import           Game
import           Graphics
import           Physics
import           Server

level :: [Box]
level =
  [((-27,-16),(-26,15))
  ,((26,-15),(27,15))
  ,((-26,-16),(27,-15))
  ,((-27,15),(27,16))
  ,((-17,-10),(-16,10))
  ,((-16,-10),(-1,-9))
  ,((-22,-5),(-17,-4))
  ,((-26,4),(-20,5))
  ,((4,-11),(19,-10))
  ,((13,-10),(14,0))
  ,((-6,-2),(4,-1))
  ,((8,6),(22,7))
  ,((8,7),(9,11))
  ,((-10,8),(-9,15))
  ,((0,-1),(1,3))
  ,((21,-2),(26,-1))]

main :: IO ()
main =
  do gameChan <- newGameChan
     renderChan <- newRenderChan
     let game = newGame level
     _ <- forkIO $ runGame gameChan renderChan game
     _ <- forkIO $ runServer gameChan
     renderMain renderChan
