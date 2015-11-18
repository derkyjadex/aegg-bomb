module Server (runServer) where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Game
import           Network.Socket
import           System.IO
import           Text.Printf

runServer :: GameChan -> IO ()
runServer gameChan =
  bracket (socket AF_INET Stream 0)
          close
          (\sock ->
             do setSocketOption sock ReuseAddr 1
                bindSocket sock
                           (SockAddrInet 4242 iNADDR_ANY)
                listen sock 2
                forever $ runAccept gameChan sock)

runAccept :: GameChan -> Socket -> IO ThreadId
runAccept gameChan sock =
  do conn <- accept sock
     forkIO $ runConnection gameChan conn

runConnection :: GameChan -> (Socket, SockAddr) -> IO ()
runConnection gameChan (sock,addr) =
  do h <- socketToHandle sock ReadWriteMode
     hSetBuffering h NoBuffering
     hPutStrLn h "Hi, what's your name?"
     name <- hGetLine h
     hPrintf h "Welcome, %s!\n" name
     playerChan <- newChan
     writeChan gameChan $ AddPlayer name playerChan
     reader <-
       forkIO . forever $
       do msg <- readChan playerChan
          case msg of
            CurrentPos pos -> hPutStrLn h $ "Current position " ++ show pos
            Health health -> hPutStrLn h $ "Health " ++ show health
            PlayersSeen players ->
              hPutStrLn h $ "Can see players " ++ show players
            EggsSeen eggs -> hPutStrLn h $ "Can see eggs " ++ show eggs
            Removed -> hClose h
     handle (\ex@(SomeException _) -> return ()) . forever $
       do cmd <- maybeRead <$> hGetLine h
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
    [(x,"")] -> Just x
    _ -> Nothing
