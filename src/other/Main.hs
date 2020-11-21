module Main (main) where

import System.Environment ( getArgs )
import System.Daemon
    ( ensureDaemonRunning, DaemonOptions(daemonPort) )
import System.Posix.Daemon ( kill )
import System.FilePath ( (</>), (<.>) )
import System.Directory ( getHomeDirectory )
import Control.Exception ( bracket )
import Control.Concurrent ( threadDelay )

import Data.Default ( def )
import Data.String ( fromString )

stopDaemon :: IO String
stopDaemon =
  bracket
    (do
      home <- getHomeDirectory
      let pidfile = home </> ("." ++ "daemon-test") <.> "pid"
      return pidfile
    )
    kill
    (return . show)

startDaemon :: DaemonOptions -> (String -> IO String) -> IO String
startDaemon opt prog = do
  ensureDaemonRunning "daemon-test" opt prog
  return "Started."

printAndSleep :: String -> IO String
printAndSleep arg = do
  threadDelay 1000000
  print $ arg ++ "."
  return arg

main :: IO ()
main = do
  let options = def { daemonPort = 7856 }
  args <- getArgs
  let args' = map fromString args
  v <- case args' of
    ["start"] -> startDaemon options printAndSleep
    ["stop"]  -> stopDaemon
    _         -> return $ "invalid command" ++ show args'
  print $ "Done: " ++ v