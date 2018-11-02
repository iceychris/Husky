{-# LANGUAGE OverloadedStrings #-}

module Main where

-- builtins
import Control.Applicative hiding ((<|>))
import Control.Arrow
--import Control.Monad.RWS
import System.IO
import System.Exit
import Data.Maybe
import Data.Int
import Data.Complex
import GHC.Float
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, putMVar, MVar)

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

-- local libs
import Util


-- TODO
-- OK make terminating via Ctrl-C possible
    -- OK (1) start vty watcher thread
        -- OK use nextEvent to get a new event in a blocking fashion
    -- OK (2) check for MVar 
-- remove charsFade when input value < 3
-- fix output dimensions
    -- (3) get terminal size 


-- constants
samplerate = 44100
bufferchunk = 4096 -- 1024
fftinput = (fst (bufferchunk `divMod` 4)) :: Int
transform = I.dftR2C
myplan = plan transform fftinput 

volMaxChars = 0.3
barWidth = 2 -- for now only 1 works?
binsToTake = 60
-- charsFill = '#' -- '█'
charsFill = '█'
-- charsFade = "|*." --"▓▒░"
charsFade = "▓▒░"
-- charsEmpty = '0'
charsEmpty = ' '
maxBarLen = 15 -- height

title = "Husky"
description = "Audio Visualizer"
bufattr = Just $ BufferAttr Nothing Nothing Nothing Nothing $ Just samplerate 

-- old
maxfft = 40



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
bar :: Int -> String
bar n | n < 3  = chF ++ (replicate (maxBarLen-n) charsEmpty)
      | n >= 3 && n <= maxBarLen = filled ++ charsFade ++ replicate (maxBarLen-n) charsEmpty 
      | n > maxBarLen = replicate maxBarLen charsFill
    where
        chF = reverse $ take n $ reverse charsFade
        filled = replicate (n-3) charsFill



--
strBar :: Int -> [String]
strBar n = map (\c -> [c]) (bar n)

-- value, maximum
vbar :: Float -> Float -> IO ()
vbar val maxi = putStrLn $ bar $ displayable val maxi




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

mline :: Int -> Char -> Image
mline wdh c = string defAttr (replicate wdh c)

volumefft3 :: V.Vector Float -> Image 
volumefft3 vec = do
    foldt (\a b -> a <|> b) (head images) (tail images)
    where
        wdh = binsToTake * barWidth
        doubles = toDouble vec
        ffts = fft doubles
        slice = V.take binsToTake ffts
        lslice = V.toList slice
        images = map (\val -> valToImage val) lslice


centerRect :: (Int, Int) -> Image -> Image
centerRect (w, h) img = translate tx ty img
    where
        tx = fst $ (w - (binsToTake * barWidth)) `divMod` 2 
        ty = fst $ (h - maxBarLen) `divMod` 2


animate :: Vty -> ByteString -> IO ()
animate vty bs = do
    region <- displayBounds $ outputIface vty
    update vty $ picForImage (centerRect region (vertJoin fftoutput qline))
    where
        floats = bytesToFloats bs
        fftoutput = volumefft3 floats
        c = (binsToTake*barWidth)
        qline = mline c charsFill



handleIOEvent :: IO Event -> IO ()
handleIOEvent x = do
    return () 

-- loop forever reading audio :)
capture :: MVar Event -> Vty -> Simple -> Float -> IO ()
capture ioBox vty s prevVol = do
    -- (3) get terminal size 
    -- (2) check for MVar 
    -- ev :: Maybe (IO Event)
    ev <- tryTakeMVar ioBox
    case ev of 
        Nothing -> do
            -- handleIOEvent x
            (readSample s) >>= (animate vty)
            capture ioBox vty s prevVol
        Just x -> do
            -- free and return
            if shouldAbort x 
                then gracefulShutdown vty s
                else capture ioBox vty s prevVol

gracefulShutdown :: Vty -> Simple -> IO ()
gracefulShutdown vty s = do
    shutdown vty
    simpleFree s


shouldAbort :: Event -> Bool
shouldAbort ev = case ev of 
    (EvKey (KChar 'c') [MCtrl]) -> True
    (EvKey (KChar 'q') _) -> True
    _ -> False


watchForIOEvents :: MVar Event -> Vty -> IO ()
watchForIOEvents ioBox vty = do
    ev <- nextEvent vty
    putStrLn $ show ev

    -- sent over to other thread
    putMVar ioBox $ ev 

    -- pattern match for Ctrl-C 
    if shouldAbort ev
        then exitWith ExitSuccess
        else watchForIOEvents ioBox vty



main = do

    -- communication between UI and recorder thread
    ioBox <- newEmptyMVar

    -- simple pulse object
    s <- simpleNew Nothing title Record Nothing description 
      (SampleSpec (F32 LittleEndian) samplerate 1) Nothing bufattr

    -- create vty
    vty <- mkVty defaultConfig

    -- (1) start vty watcher thread
    forkIO $ capture ioBox vty s 1.0

    -- watch for IO Events
    watchForIOEvents ioBox vty


