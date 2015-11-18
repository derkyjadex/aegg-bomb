module Graphics
       (RenderChan(..), sendScene, Scene(..), renderMain, newRenderChan)
       where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Maybe
import           Physics
import           System.IO

data Scene =
  Scene {_walls      :: [Box]
        ,_players    :: [(String,Pos,Box)]
        ,_eggs       :: [(Pos,Box,Double)]
        ,_explosions :: [(Pos,Box)]}
  deriving (Show)

data RenderChan = RenderChan (MVar Scene)

newRenderChan :: IO RenderChan
newRenderChan = RenderChan <$> newMVar (Scene [] [] [] [])

sendScene :: RenderChan -> Scene -> IO Scene
sendScene (RenderChan var) = swapMVar var

readScene :: RenderChan -> IO Scene
readScene (RenderChan var) = readMVar var

wallString :: Box -> String
wallString wall = "Wall " ++ show wall

playerString :: (String, Pos, Box) -> String
playerString player = "Player " ++ show player

eggString :: (Pos, Box, Double) -> String
eggString egg = "Egg " ++ show egg

explosionString :: (Pos, Box) -> String
explosionString explosion = "Explosion " ++ show explosion

renderScene :: Scene -> IO ()
renderScene (Scene walls players eggs explosions) =
  do putStrLn ":begin"
     mapM_ (putStrLn . wallString) walls
     mapM_ (putStrLn . playerString) players
     mapM_ (putStrLn . eggString) eggs
     mapM_ (putStrLn . explosionString) explosions
     putStrLn ":end"

renderMain :: RenderChan -> IO ()
renderMain chan =
  do scene <- readScene chan
     renderScene scene
     threadDelay $ 500 * 1000
     renderMain chan
