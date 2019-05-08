{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror -Wno-type-defaults #-}

module Talk
    ( threadTalk
    , threadReceive
    ) where

import Control.Concurrent.Async (cancel, wait)
import Control.Concurrent.STM.TQueue (newTQueueIO)
import Control.Exception.Lifted (finally)
import Control.Monad.IO.Class (liftIO)
import GHC.Stack (HasCallStack)
import Network.Simple.TCP (SockAddr)
import Server
import System.IO (BufferMode(LineBuffering), Handle, Newline(CRLF), NewlineMode(NewlineMode, inputNL, outputNL), hClose, hIsEOF, hSetBuffering, hSetEncoding, hSetNewlineMode, latin1)
import ThreadUtils
import TextUtils
import Types
import Utils
import qualified Data.Text.IO as T

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
