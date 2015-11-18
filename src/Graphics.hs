{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Graphics
       (RenderChan(..), sendScene, Scene(..), renderMain, newRenderChan,
        runWebsocketServer)
       where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson         (ToJSON)
import qualified Data.Aeson         as Aeson
import           GHC.Generics
import qualified Network.WebSockets as WS
import           Physics
import           Text.Show.Pretty

data Scene =
  Scene {_walls      :: [Box]
        ,_players    :: [(String,Pos,Box)]
        ,_eggs       :: [(Pos,Box,Double)]
        ,_explosions :: [(Pos,Box)]}
  deriving (Show,Generic,ToJSON)

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
  forever $ do scene <- readScene chan
               renderScene scene
               threadDelay $ 500 * 1000

renderToWebsocket :: RenderChan -> WS.Connection -> IO ()
renderToWebsocket renderChan conn =
  forever $
  do scene <- readScene renderChan
     WS.send conn $ WS.DataMessage $ WS.Text $ Aeson.encode scene
     threadDelay $ 500 * 1000

runWebsocket :: RenderChan -> WS.ServerApp
runWebsocket renderChan pendingConnection =
  do conn <- WS.acceptRequest pendingConnection
     forever $ renderToWebsocket renderChan conn

runWebsocketServer :: RenderChan -> IO ()
runWebsocketServer chan = WS.runServer "127.0.0.1" 8000 $ runWebsocket chan
