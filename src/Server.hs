module Server where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Game

runServer :: GameChan -> IO ()
runServer gameChan =
  bracket (socket AF_INET Stream 0)
          (close)
          (\sock -> do
              setSocketOption sock ReuseAddr 1
              bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
              listen sock 2
              runAccept sock gameChan)

runAccept :: Socket -> Chan GameMsg -> IO ()
runAccept sock chan = do
  conn <- accept sock
  forkIO $ runConnection conn chan
  runAccept sock chan

runConnection :: (Socket, SockAddr) -> Chan GameMsg -> IO ()
runConnection (sock, addr) gameChan = do
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  hPutStrLn h "Hi, what's your name?"
  name <- hGetLine h
  hPutStrLn h $ "Welcome, " ++ name ++ "!"
  playerChan <- newChan
  writeChan gameChan $ AddPlayer name playerChan

  reader <- forkIO $ forever $ do
    msg <- readChan playerChan
    case msg of
      CurrentPos pos -> hPutStrLn h $ "Current position " ++ show pos
      PlayersSeen players -> hPutStrLn h $ "Can see players " ++ show players
      EggsSeen eggs -> hPutStrLn h $ "Can see eggs " ++ show eggs
      Removed -> hClose h

  handle (\ex@(SomeException _) -> return ()) $ forever $ do
    cmd <- liftM maybeRead $ hGetLine h
    case cmd of
      Just (Move vel) -> writeChan gameChan $ MovePlayer name vel
      Just (Throw vel) -> writeChan gameChan $ ThrowEgg name vel
      Nothing -> hPutStrLn h "Unknown command"

  killThread reader
  writeChan gameChan $ RemovePlayer name
  hClose h

maybeRead :: Read a => String -> Maybe a
maybeRead s =
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
