module Graphics
       (RenderChan(..), sendScene, Scene(..), renderMain, newRenderChan)
       where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Maybe
import           Physics
import           System.IO
import           Text.Show.Pretty

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

renderScene :: Scene -> IO ()
renderScene scene = putStrLn (ppShow scene)

renderMain :: RenderChan -> IO ()
renderMain chan =
  do scene <- readScene chan
     renderScene scene
     threadDelay $ 500 * 1000
     renderMain chan
