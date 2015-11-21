{-# LANGUAGE OverloadedStrings #-}
module Graphics
       (RenderChan(..), sendScene, Scene(..), renderMain, newRenderChan)
       where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           Data.HashMap.Strict  (fromList)
import qualified Network.WebSockets   as WS
import           Physics

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

wallJson :: Box -> Value
wallJson wall =
  Object $ fromList [ ("type", "wall")
                    , ("bounds", toJSON wall)
                    ]

playerJson :: (String, Pos, Box) -> Value
playerJson (name, pos, bounds) =
  Object $ fromList [ ("type", "player")
                    , ("name", toJSON name)
                    , ("pos", toJSON pos)
                    , ("bounds", toJSON bounds)
                    ]

eggJson :: (Pos, Box, Double) -> Value
eggJson (pos, bounds, height) =
  Object $ fromList [ ("type", "egg")
                    , ("pos", toJSON pos)
                    , ("bounds", toJSON bounds)
                    , ("height", toJSON height)
                    ]

explosionJson :: (Pos, Box) -> Value
explosionJson (pos, bounds) =
  Object $ fromList [ ("type", "explosion")
                    , ("pos", toJSON pos)
                    , ("bounds", toJSON bounds)
                    ]

sceneJson :: Scene -> Value
sceneJson (Scene walls players eggs explosions) =
  toJSON $
  fmap wallJson walls ++
  fmap playerJson players ++
  fmap eggJson eggs ++
  fmap explosionJson explosions

runConnection :: RenderChan -> WS.Connection -> IO ()
runConnection chan conn =
  forever $
  do scene <- readScene chan
     WS.sendTextData conn $ encode $ sceneJson scene
     threadDelay (500 * 1000)

runWebsocket :: RenderChan -> WS.ServerApp
runWebsocket chan pending =
  do conn <- WS.acceptRequest pending
     WS.forkPingThread conn 30
     runConnection chan conn

renderMain :: RenderChan -> IO ()
renderMain chan =
  WS.runServer "0.0.0.0" 2424 $ runWebsocket chan
