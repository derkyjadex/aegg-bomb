{-# LANGUAGE OverloadedStrings #-}
module Server (runServer) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Game
import           Network.Socket
import           System.IO
import           Text.Printf
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)

playerMsgJson :: PlayerMsg -> Value
playerMsgJson (Status pos health players eggs) =
  object ["pos" .= pos
         ,"health" .= (100 * ceiling health :: Int)
         ,"playersSeen" .= fmap playerJson players
         ,"eggsSeen" .= fmap eggJson eggs]
  where playerJson (name,pos) =
          object ["name" .= name
                 ,"pos" .= pos]
        eggJson pos =
          object ["pos" .= pos]

playerMsgJson (Killed player) =
  object ["killedBy" .= player]

playerMsgJson Removed =
  messageJson "You have been removed from the game"

messageJson :: String -> Value
messageJson message =
  object ["message" .= T.pack message]

startConnection :: GameChan -> WS.Connection -> IO (String,Chan PlayerMsg)
startConnection gameChan conn =
  do WS.Text nameBytes <- WS.receiveDataMessage conn
     let name = T.unpack $ WS.fromLazyByteString nameBytes
     playerChan <- newChan
     writeChan gameChan $ AddPlayer name playerChan
     return (name,playerChan)

runConnection :: GameChan -> WS.Connection -> IO ()
runConnection gameChan conn =
  do (name,playerChan) <- startConnection gameChan conn
     reader <-
       forkIO . forever $
       do msg <- readChan playerChan
          WS.sendTextData conn $ encode $ playerMsgJson msg
          when (msg == Removed) $ WS.sendClose conn $ T.pack ""
     handle (\(SomeException _) -> return ()) . forever $
       do WS.Text cmdBytes <- WS.receiveDataMessage conn
          let cmd = T.unpack $ WS.fromLazyByteString cmdBytes
          case maybeRead cmd of
            Just (Move x y) -> writeChan gameChan (MovePlayer name (x,y))
            Just (Throw x y) -> writeChan gameChan (ThrowEgg name (x,y))
            Nothing -> WS.sendTextData conn $ encode $ messageJson ("Unknown command: '" ++ cmd ++ "'")
     killThread reader
     writeChan gameChan (RemovePlayer name)

runWebsocket :: GameChan -> WS.ServerApp
runWebsocket chan pending =
  do conn <- WS.acceptRequest pending
     WS.forkPingThread conn 30
     runConnection chan conn

runServer :: GameChan -> IO ()
runServer chan =
  let (address,port) = ("0.0.0.0",4242)
  in do printf "Running player server on: %s:%d\n" address port
        WS.runServer address port $ runWebsocket chan

maybeRead :: Read a => String -> Maybe a
maybeRead s =
  case reads s of
    [(x,"")] -> Just x
    _ -> Nothing
