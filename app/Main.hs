{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Main (main) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async, cancel, wait)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception (AsyncException(..), SomeException, fromException)
import Control.Exception.Lifted (finally, handle)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.IORef (newIORef)
import Data.Monoid ((<>))
import GHC.Stack (HasCallStack)
import Network.Socket (socketToHandle)
import Network.Simple.TCP (HostPreference(HostAny), ServiceName, SockAddr, accept, listen)
import Server
import System.IO (BufferMode(LineBuffering), Handle, Newline(CRLF), NewlineMode(NewlineMode, inputNL, outputNL), IOMode(ReadWriteMode), hClose, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import TextUtils
import ThreadUtils
import Types
import Utils
import qualified Data.Text as T
import qualified Data.Text.IO as T (hGetLine, putStrLn)

{-
To connect:
brew install telnet
telnet localhost 9696
-}

{-
Keep in mind that in Haskell, killing a parent thread does NOT kill its children threads!
Threads must be manually managed via the "Async" library. This takes careful thought and consideration.
Threads should never be "leaked:" we don't ever want a situation in which a child thread is left running and no other threads are aware of it.
Of course, the app should be architected in such a way that when an exception is thrown, it is handled gracefully. First and foremost, an exception must be caught on/by the thread on which the exception was thrown. If the exception represents something critical or unexpected (it's a bug, etc. - this is the vast majority of exceptions that we'll encounter in practice), and the exception occurs on a child thread, then the child thread should rethrow the exception to the listen (main) thread. The listen thread's exception handler should catch the rethrown exception and gracefully shut down, manually killing all child threads in the process.
-}

{-
TODO:
To fix the thread leakage bug:
* The "/throw" command should throw an exception on the server thread.
* The server thread's exception handler should catch the exception and throw an exception to the listen thread.
* The listen thread's exception handler should catch the exception and gracefully shut the server down by doing the following:
1) Put a "Msg" in every "MsgQueue" indicating that the server is shutting down.
2) Wait for every talk thread to finish.
-}

initChatState :: ChatState
initChatState = ChatState Nothing

main :: HasCallStack => IO ()
main = runReaderT threadListen =<< newIORef initChatState

-- This is the main thread. It listens for incoming connections.
threadListen :: HasCallStack => ChatStack ()
threadListen = liftIO myThreadId >>= \ti -> do
    modifyState $ \cs -> (cs { listenThreadId = Just ti }, ())
    liftIO . T.putStrLn $ "Welcome to the Haskell Chat Server!"
    listenHelper `finally` bye
  where
    bye = liftIO . T.putStrLn . nl $ "Goodbye!"

listenHelper :: HasCallStack => ChatStack ()
listenHelper = handle listenExHandler $ ask >>= \env ->
    let listener = liftIO . listen HostAny port $ accepter
        accepter (serverSocket, _) = forever . accept serverSocket $ talker
        talker (clientSocket, remoteAddr) = do
            T.putStrLn . T.concat $ [ "Connected to ", showTxt remoteAddr, "." ]
            h <- socketToHandle clientSocket ReadWriteMode
            void . async . runReaderT (threadTalk h remoteAddr) $ env -- TODO: Store the talk thread's "Async" data in the shared state.
    in listener

listenExHandler :: HasCallStack => SomeException -> ChatStack ()
listenExHandler e = case fromException e of
  Just UserInterrupt -> liftIO . T.putStrLn $ "Exiting on user interrupt."
  _                  -> error famousLastWords -- This throws another exception. The stack trace is printed.
  where
    famousLastWords = "panic! (the 'impossible' happened)"

-- This thread is spawned for every incoming connection.
-- Its main responsibility is to spawn a "receive" and a "server" thread.
threadTalk :: HasCallStack => Handle -> SockAddr -> ChatStack ()
threadTalk h addr = talk `finally` liftIO cleanUp
  where
    -- TODO: Handle exceptions.
    talk = liftIO newTQueueIO >>= \mq -> do
        liftIO configBuffer
        (a, b) <- (,) <$> runAsync (threadReceive h mq) <*> runAsync (threadServer h mq)
        liftIO $ wait b >> cancel a
    configBuffer = hSetBuffering h LineBuffering >> hSetNewlineMode h nlMode >> hSetEncoding h latin1
    nlMode       = NewlineMode { inputNL = CRLF, outputNL = CRLF }
    cleanUp      = T.putStrLn ("Closing the handle for " <> showTxt addr <> ".") >> hClose h

-- This thread polls the handle for the client's connection. Incoming text is sent down the message queue.
threadReceive :: HasCallStack => Handle -> MsgQueue -> ChatStack () -- TODO: Handle exceptions.
threadReceive h mq = mIf (liftIO . hIsEOF $ h) (writeMsg mq Dropped) $ do
    receive mq =<< liftIO (T.hGetLine h)
    threadReceive h mq

port :: ServiceName
port = "9696"

