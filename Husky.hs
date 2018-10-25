{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Data.Maybe
import Data.Int
import Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as V

import Sound.Pulse.Simple


-- TODO
-- - add signal handler to call simpleFree at some point... 
-- - make capture take a function like (B.ByteString -> IO a)
--


-- make calculations on data possible
bytesToFloats :: BS.ByteString -> V.Vector Float
bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

samplerate = 44100
bufferchunk = 4096 -- 1024

readSample :: Simple -> IO ByteString 
readSample s = simpleReadRaw s bufferchunk :: IO ByteString 

vecAbs :: V.Vector Float -> V.Vector Float
vecAbs vec = V.map (\v -> abs v) vec 


-- helper function to print a [Float]
-- for printing IO [Float]
-- is this needed?
-- or can I somehow compose show with putStrLn?
putStrLnFloat :: ByteString -> IO ()
putStrLnFloat bytes = do
    System.IO.putStrLn $ show $ V.maximum $ vecAbs $ bytesToFloats bytes


volMaxChars = 40
volume :: ByteString -> IO ()
volume bs = do
    sequence $ map (\x -> putStr "█") $ replicate n 0 
    putStrLn "▓▒░"
    where
        floatmax = V.maximum $ vecAbs $ bytesToFloats bs
        n = round $ floatmax * volMaxChars




-- loop forever reading audio :)
capture :: Simple -> Float -> IO ()
capture s prevVol = do
    -- (readSample s) >>= putStrLnFloat
    (readSample s) >>= volume
    capture s prevVol


main = do
    s <- simpleNew Nothing title Record Nothing description 
      (SampleSpec (F32 LittleEndian) samplerate 1) Nothing bufattr
    capture s 1.0
    simpleFree s
    where
        title = "Husky"
        description = "Audio Visualizer"
        bufattr = Just $ BufferAttr Nothing Nothing Nothing Nothing $ Just samplerate 
