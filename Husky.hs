{-# LANGUAGE OverloadedStrings #-}

module Main where

-- builtins
import System.IO
import Data.Maybe
import Data.Int
import Data.Complex
import GHC.Float

-- 3rd party
import Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as V
import Sound.Pulse.Simple
import Numeric.FFT.Vector.Plan
import Numeric.FFT.Vector.Unitary
import qualified Numeric.FFT.Vector.Invertible as I


-- TODO
-- - add signal handler to call simpleFree at some point... 


-- constants
samplerate = 44100
bufferchunk = 4096 -- 1024
transform = I.dftR2C
myplan = plan transform (bufferchunk :: Int)




-- make calculations on data possible
bytesToFloats :: BS.ByteString -> V.Vector Float
bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

toDouble :: V.Vector Float -> V.Vector Double
toDouble vec = V.map (\x -> float2Double x) vec

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

-- value, maximum
vbar :: Float -> Int -> IO ()
vbar val maxi = do
    putStr $ replicate n '█'
    putStrLn "▓▒░"
    where
        n = round $ (*) val $ fromIntegral maxi


fft :: V.Vector Double -> V.Vector Double
fft floats = V.map magnitude resfft
    where 
        resfft = execute myplan floats

volMaxChars = 40
volume :: ByteString -> IO ()
volume bs = vbar fmax volMaxChars 
    where
        fmax = V.maximum $ vecAbs $ bytesToFloats bs

maxfft = 40
volumefft :: ByteString -> IO ()
volumefft bs = vbar headFloats maxfft
    where
        doubles = (.) toDouble bytesToFloats bs
        headFloats = double2Float $ V.head doubles


-- loop forever reading audio :)
capture :: Simple -> Float -> IO ()
capture s prevVol = do
    -- (readSample s) >>= putStrLnFloat
    -- (readSample s) >>= volume
    (readSample s) >>= volumefft
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
