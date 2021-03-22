#!/usr/bin/runhaskell -threaded
module Main where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import           Control.Monad           (forM_, forever, when)
import qualified Data.ByteString.Lazy    as LBS
import           MIDI.GCode              (midi2gcode)
import           System.IO               (BufferMode (LineBuffering), Handle,
                                          IOMode (..), hGetLine, hPutStr,
                                          hSetBuffering, openBinaryFile, stdout)


readWorker :: MVar String -> Handle -> IO ()
readWorker mtx readH = forever $ do
  line <- hGetLine readH
  when (line == "ok") $
    putMVar mtx line


sendToSerial :: String -> [String] -> IO ()
sendToSerial port gcode = do
  hSetBuffering stdout LineBuffering
  mtx <- newMVar "Starting..."

  readH <- openBinaryFile port ReadMode
  _ <- forkIO (readWorker mtx readH)

  writeH <- openBinaryFile port WriteMode

  forM_ gcode $ \code -> do
    res <- takeMVar mtx
    putStrLn res
    putStrLn code
    hPutStr writeH $ code ++ "\n"


main :: IO ()
main = do
  let midiFile = "music/house.mid"

  gcode <- midi2gcode <$> LBS.readFile midiFile

  -- sendToSerial "/dev/ttyS5" gcode
  writeFile "output.nc" $ unlines gcode
