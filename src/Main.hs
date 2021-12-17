module Main where

import Control.Monad (when)
import Data.ByteString.Internal (packChars)
import Data.ByteString.Lazy.Internal (unpackChars)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)

import Solutions (solveDay)

inputForDay :: Int -> IO String
inputForDay day = do
  let dir  = "./inputs/"
  let path = dir ++ show day ++ ".txt"
  createDirectoryIfMissing False dir
  alreadyExists <- doesFileExist path
  if alreadyExists
    then readFile path
    else do
      input <- fetchInput day
      writeFile path input
      return input

fetchInput :: Int -> IO String
fetchInput day = do
  session <- getEnv "AOC_SESSION"
  let cookie = packChars $ "session=" ++ session

  let url    = "https://adventofcode.com/2021/day/" ++ show day ++ "/input"
  req' <- parseRequest url
  let req = req' { requestHeaders = (hCookie, cookie) : requestHeaders req' }

  manager  <- newManager tlsManagerSettings
  response <- httpLbs req manager
  return $ unpackChars (responseBody response)

main :: IO ()
main = do
  args <- getArgs
  when (null args) (putStrLn "Usage: solve [DAY]" >> exitFailure)

  let day = read $ head args
  input <- inputForDay day
  putStrLn $ "Day " ++ show day ++ " solution"
  solveDay day input
