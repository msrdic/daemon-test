{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-
This is based on an example from the original daemons repository, a simple
in-memory key-value store. I've just expanded this so it takes an additional
command to stop the daemon. Not particularly useful, but wanted to see how
that works.
-}
module Main where

import Control.Concurrent.MVar ( MVar, newMVar, modifyMVar )
import Data.ByteString.Char8 ( ByteString )
import Data.Default ( def )
import Data.Serialize ( Serialize )
import Data.String ( fromString )
import qualified Data.Map as M
import GHC.Generics ( Generic )
import System.Environment ( getArgs )
import System.Daemon
    ( ensureDaemonRunning, runClient, DaemonOptions(daemonPort) )
import System.Posix.Daemon ( kill )
import System.FilePath ( (</>), (<.>) )
import System.Directory ( getHomeDirectory )
import Control.Exception (bracket)

data Command = Put ByteString ByteString
             | Get ByteString
             | Kill
               deriving ( Generic, Show )

instance Serialize Command

data Response = Failed String
              | Value ByteString
              | NOP String
                deriving ( Generic, Show )

instance Serialize Response

type Book = M.Map ByteString ByteString

handleCommand :: MVar Book -> Command -> IO Response
handleCommand bookVar comm =
  case comm of
    Kill ->
      bracket
        (do
          home <- getHomeDirectory
          let pidfile = home </> ("." ++ "memokv") <.> "pid"
          return pidfile
        )
        kill
        (return . NOP)
    _ ->
          modifyMVar bookVar $ \book -> return $
            case comm of
              Get key -> ( book
                        , maybe (Failed "not found") Value (M.lookup key book) )
              Put key value -> ( M.insert key value book
                              , Value "ok" )

main :: IO ()
main = do
    bookVar <- newMVar M.empty
    let options = def { daemonPort = 7856 }
    ensureDaemonRunning "memokv" options (handleCommand bookVar)
    args <- getArgs
    let args' = map fromString args
    res <- case args' of
      ["get", key]        -> runClient "localhost"  7856 (Get key)
      ["put", key, value] -> runClient "localhost"  7856 (Put key value)
      ["kill"]            -> runClient "localhost"  7856 Kill
      _                   -> error "invalid command"
    print (res :: Maybe Response)
