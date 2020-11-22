{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import System.Environment ( getArgs )
import System.Daemon (Port,  ensureDaemonRunning, DaemonOptions( daemonPort ), runClient )
import System.Posix.Daemon ( kill )
import System.FilePath ( (</>), (<.>) )
import System.Directory ( getHomeDirectory )
import Control.Exception ( bracket )

import Data.Default ( def )
import Data.String ( fromString )
import GHC.Generics (Generic)
import Data.Serialize ( Serialize )
import Data.Maybe ( fromMaybe )

port :: Port
port = 7856

data Command = Start | Status | Stop deriving (Show, Generic)
instance Serialize Command

data DaemonStatus = Running | Stopped | NA deriving (Show, Generic)
instance Serialize DaemonStatus

getPidFilePath :: IO FilePath
getPidFilePath = do
  home <- getHomeDirectory
  return $ home </> ("." ++ "daemon-test") <.> "pid"

stopDaemon :: IO DaemonStatus
stopDaemon =
  bracket
    getPidFilePath
    kill
    (`seq` return Stopped)

startDaemon :: (Serialize a, Serialize b) => DaemonOptions -> (a -> IO b) -> IO DaemonStatus
startDaemon opt prog = do
  ensureDaemonRunning "daemon-test" opt prog
  return Running

daemonStatus :: IO DaemonStatus
daemonStatus = do
  result <- runClient "localhost" port Status
  return $ fromMaybe NA result

printAndSleep :: Command -> IO DaemonStatus
printAndSleep command =
  case command of
    Status -> return Running
    _      -> return NA

main :: IO ()
main = do
  let options = def { daemonPort = port }
  args <- getArgs
  let args' = map fromString args
  v <- case args' of
    ["start"]  -> startDaemon options printAndSleep
    ["stop"]   -> stopDaemon
    ["status"] -> daemonStatus
    _          -> error $ "invalid command" ++ show args'
  putStrLn $ "Done: " ++ show v