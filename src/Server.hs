module Server where

import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Graphics
import Game

runServer :: RenderChan -> IO ()
runServer renderChan = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  gameChan <- newChan
  game <- forkIO $ runGame gameChan renderChan newGame
  acceptor <- forkIO $ runAccept sock gameChan
  runConsole
  writeChan gameChan Stop
  killThread game
  killThread acceptor
  close sock

runConsole :: IO ()
runConsole = do
  putStr "> "
  cmd <- getLine
  case cmd of
    "quit" -> return ()
    _ -> do
      putStrLn "Unknown command"
      runConsole

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
      NothingSeen pos -> hPutStrLn h $ "Nothing seen from " ++ show pos
      Removed -> hClose h

  handle (\ex@(SomeException _) -> return ()) $ forever $ do
    cmd <- liftM maybeRead $ hGetLine h
    case cmd of
      Just (Move vel) -> writeChan gameChan $ MovePlayer name vel
      Nothing -> hPutStrLn h "Unknown command"

  killThread reader
  writeChan gameChan $ RemovePlayer name
  hClose h

maybeRead :: Read a => String -> Maybe a
maybeRead s =
  case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
