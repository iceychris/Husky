{-# LANGUAGE OverloadedStrings #-}

module Main where

-- builtins
import Control.Applicative hiding ((<|>))
import Control.Arrow
--import Control.Monad.RWS
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

import Graphics.Vty
import Graphics.Vty.Attributes
import Graphics.Vty.Inline
import Graphics.Vty.Picture
import Graphics.Vty.Output


-- TODO
-- - add signal handler to call simpleFree at some point... 
-- - make terminating via Ctrl-C possible
-- - remove charsFade when input value < 3


-- constants
samplerate = 44100
bufferchunk = 1024 -- 1024
fftinput = (fst (bufferchunk `divMod` 4)) :: Int
transform = I.dftR2C
myplan = plan transform fftinput 

volMaxChars = 0.07
ab = 0.1
barWidth = 2 -- for now only 1 works?
binsToTake = 64
-- charsFill = '#' -- '█'
charsFill = '█'
-- charsFade = "|*." --"▓▒░"
charsFade = "▓▒░"


-- old
maxfft = 40


foldt            :: (a -> a -> a) -> a -> [a] -> a
foldt f z []     = z
foldt f z [x]    = x
foldt f z xs     = foldt f z (pairs f xs)
 
foldi            :: (a -> a -> a) -> a -> [a] -> a
foldi f z []     = z
foldi f z (x:xs) = f x (foldi f z (pairs f xs))
 
pairs            :: (a -> a -> a) -> [a] -> [a]
pairs f (x:y:t)  = f x y : pairs f t
pairs f t        = t

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

-- 
maxBarLen = 30
bar :: Int -> String
bar n = a ++ b ++ replicate (maxBarLen-n) ' '
    where
        a = replicate n charsFill
        b = charsFade

--
strBar :: Int -> [String]
strBar n = map (\c -> [c]) (bar n)

-- value, maximum
vbar :: Float -> Float -> IO ()
vbar val maxi = putStrLn $ bar $ displayable val maxi


-- convert a float with a maximum to an int
-- ?
displayable :: Float -> Float -> Int
displayable val maxi = round $ (*) val $ maxi



-- vertically concatenated bar, e.g.
-- ░░░
-- ▒▒▒
-- ▓▓▓
-- ███
-- ███
-- ███
imbar :: Int -> Int -> Image
imbar n width = horizCat $ replicate width columns 
    where
        onebar = reverse (strBar n)
        columns = vertCat (map (\x -> string defAttr x) onebar)



fft :: V.Vector Double -> V.Vector Double
fft floats = V.map magnitude resfft
    where 
        resfft = execute myplan floats

volume :: ByteString -> IO ()
volume bs = vbar fmax volMaxChars 
    where
        fmax = V.maximum $ vecAbs $ bytesToFloats bs

volumefft :: ByteString -> IO ()
volumefft bs = vbar headFloats maxfft
    where
        doubles = (.) toDouble bytesToFloats bs
        headFloats = double2Float $ V.head doubles


volumefft2 :: Int -> IO ()
volumefft2 val = do
    t <- standardIOConfig >>= outputForConfig
    reserveDisplay t
    (w,h) <- displayBounds t
    let row0 = replicate (fromEnum w) 'X' ++ "\n"
        rowH = replicate (fromEnum w - 1) 'X'
        rowN = "X" ++ replicate (fromEnum w - 2) ' ' ++ "X\n"
        image = row0 ++ 
            (concat $ replicate (fromEnum (fst (h `divMod` 2)) - 2) rowN) ++ 
            (bar val) ++ "\n" ++
            (concat $ replicate (fromEnum (fst (h `divMod` 2)) - 2) rowN) ++ 
            rowH
    putStr image
    hFlush stdout
    -- releaseDisplay t
    -- releaseTerminal t
    return ()


valToImage :: Double -> Image
valToImage val = imbar (displayable (double2Float val) volMaxChars) barWidth


volumefft3 :: V.Vector Float -> Image 
volumefft3 vec = do
    -- [Image] -> Image
    -- using <|>
    --  which is
    --   Image -> Image -> Image
    (foldt (\a b -> a <|> b) (head images) (tail images))
    -- vertCat images
    where
        doubles = toDouble vec
        ffts = fft doubles
        slice = V.take binsToTake ffts
        lslice = V.toList slice
        images = map (\val -> valToImage val) lslice



animate :: Vty -> ByteString -> IO ()
animate vty bs = do
    let pic = picForImage $ volumefft3 floats  
    update vty pic
    -- volumefft2 $ (round $ 40.0 * desired :: Int)
    where
        floats = bytesToFloats bs
    --     desired = V.maximum floats


-- loop forever reading audio :)
capture :: Vty -> Simple -> Float -> IO ()
capture vty s prevVol = do
    -- (readSample s) >>= putStrLnFloat
    -- (readSample s) >>= volume
    -- (readSample s) >>= volumefft
    (readSample s) >>= (animate vty)
    capture vty s prevVol



main = do
    s <- simpleNew Nothing title Record Nothing description 
      (SampleSpec (F32 LittleEndian) samplerate 1) Nothing bufattr
    -- create vty
    vty <- mkVty defaultConfig
    capture vty s 1.0
    simpleFree s
    where
        title = "Husky"
        description = "Audio Visualizer"
        bufattr = Just $ BufferAttr Nothing Nothing Nothing Nothing $ Just samplerate 
