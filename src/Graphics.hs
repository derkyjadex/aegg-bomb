module Graphics where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import System.IO
import Physics

data Scene = Scene [Box] [(String, Pos, Box)] [(Pos, Box)]

data RenderChan = RenderChan (MVar Scene)

newRenderChan :: IO RenderChan
newRenderChan =
  liftM RenderChan $ newMVar $ Scene [] [] []

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

renderScene :: Scene -> IO ()
renderScene (Scene walls players eggs) = do
  putStrLn ":begin"
  mapM_ (putStrLn . wallString) walls
  mapM_ (putStrLn . playerString) players
  mapM_ (putStrLn . eggString) eggs
  putStrLn ":end"

renderMain :: RenderChan -> IO ()
renderMain chan = do
  scene <- readScene chan
  renderScene scene
  threadDelay $ 500 * 1000
  renderMain chan
