{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Listen
    ( threadListen
    ) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (async)
import Control.Exception (AsyncException(..), SomeException, fromException)
import Control.Exception.Lifted (finally, handle)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, runReaderT)
import GHC.Stack (HasCallStack)
import Network.Simple.TCP (HostPreference(HostAny), ServiceName, accept, listen)
import Network.Socket (socketToHandle)
import System.IO (IOMode(ReadWriteMode))
import Talk
import TextUtils
import ThreadUtils
import Types
import qualified Data.Text as T
import qualified Data.Text.IO as T (putStrLn)

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

port :: ServiceName
port = "9696"
