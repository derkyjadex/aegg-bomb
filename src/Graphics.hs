module Graphics where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import System.IO
import Physics

data Scene = Scene [Box] [(String, Pos, Box)] [(Pos, Box)] [(Pos, Box)]

data RenderChan = RenderChan (MVar Scene)

newRenderChan :: IO RenderChan
newRenderChan =
  liftM RenderChan $ newMVar $ Scene [] [] [] []

sendScene :: RenderChan -> Scene -> IO ()
sendScene (RenderChan var) msg =
  swapMVar var msg >> return ()

readScene :: RenderChan -> IO Scene
readScene (RenderChan var) = readMVar var

wallString :: Box -> String
wallString wall =
  "Wall " ++ show wall

playerString :: (String, Pos, Box) -> String
playerString player =
  "Player " ++ show player

eggString :: (Pos, Box) -> String
eggString egg =
  "Egg " ++ show egg

explosionString :: (Pos, Box) -> String
explosionString explosion =
  "Explosion " ++ show explosion

renderScene :: Scene -> IO ()
renderScene (Scene walls players eggs explosions) = do
  putStrLn ":begin"
  mapM_ (putStrLn . wallString) walls
  mapM_ (putStrLn . playerString) players
  mapM_ (putStrLn . eggString) eggs
  mapM_ (putStrLn . explosionString) explosions
  putStrLn ":end"

renderMain :: RenderChan -> IO ()
renderMain chan = do
  scene <- readScene chan
  renderScene scene
  threadDelay $ 500 * 1000
  renderMain chan
