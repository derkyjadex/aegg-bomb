{-# LANGUAGE OverloadedStrings #-}
module Graphics
       (RenderChan(..), sendScene, Scene(..), renderMain, newRenderChan)
       where

import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import           GHC.Generics
import qualified Network.WebSockets as WS
import           Physics
import           Text.Printf

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

origin :: (Double, Double)
origin = (0, 0)

wallJson :: Box -> Value
wallJson wall =
  object ["pos" .= origin,"bounds" .= wall]

playerJson :: (String, Pos, Box) -> Value
playerJson (name,pos,bounds) =
  object ["pos" .= pos,"bounds" .= bounds,"name" .= name]

eggJson :: (Pos,Box,Double) -> Value
eggJson (pos,bounds,height) =
  object ["pos" .= pos,"bounds" .= bounds,"height" .= height]

explosionJson :: (Pos, Box) -> Value
explosionJson (pos,bounds) =
  object ["pos" .= pos,"bounds" .= bounds]

sceneJson :: Scene -> Value
sceneJson (Scene walls players eggs explosions) =
  object ["walls" .= fmap wallJson walls
         ,"players" .= fmap playerJson players
         ,"eggs" .= fmap eggJson eggs
         ,"explosions" .= fmap explosionJson explosions]

runConnection :: RenderChan -> WS.Connection -> IO ()
runConnection chan conn =
  forever $
  do scene <- readScene chan
     WS.sendTextData conn $ encode $ sceneJson scene
     threadDelay (100 * 1000)

runWebsocket :: RenderChan -> WS.ServerApp
runWebsocket chan pending =
  do conn <- WS.acceptRequest pending
     WS.forkPingThread conn 30
     runConnection chan conn

renderMain :: RenderChan -> IO ()
renderMain chan =
  let (address,port) = ("0.0.0.0",2424)
  in do printf "Running display server on: %s:%d\n" address port
        WS.runServer address port $ runWebsocket chan
