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

-- local
import Util
import Model
import Visualizers


-- TODO
-- OK make terminating via Ctrl-C possible
    -- OK (1) start vty watcher thread
        -- OK use nextEvent to get a new event in a blocking fashion
    -- OK (2) check for MVar 
-- remove charsFade when input value < 3
-- fix output dimensions
    -- (3) get terminal size 



qgracefulShutdown :: Husky -> IO ()
qgracefulShutdown h = do
    putStrLn "qgracefulShutdow not implemented"
    --(vtyInstance h) >>= shutdown
    --(recSimple h) >>= simpleFree



-- move these
volMaxChars = 0.3
barWidth = 2
binsToTake = 60
maxBarLen = 15 -- height

-- initial values
defaultBufferchunk = 4096 -- 1024
fftInputs = (fst (defaultBufferchunk `divMod` 4))
fftTransform = I.dftR2C
fftPlan = plan fftTransform fftInputs
defaultAudio = Audio {
    audioVolume = 0,
    audioSample = V.fromList $ replicate defaultBufferchunk 0.0,
    audioFFT    = V.fromList $ replicate defaultBufferchunk 0.0,
    audioFFTSq  = V.fromList $ replicate defaultBufferchunk 0.0
}
huskyDefault = Husky {
    title       = "Husky",
    description = "Audio Visualizer",

    samplerate       = 44100,
    bufferchunk      = defaultBufferchunk,

    fftInput         = fftInputs,

    window_width     = 0,
    window_height    = 0,

    charsEmpty       = ' ',
    charsFilled      = '█',
    charsFade        = "▓▒░",

    recBufattr       = Just $ BufferAttr Nothing Nothing Nothing Nothing $ Just $ samplerate huskyDefault,

    audio            = defaultAudio
}

vInfo = Visualizer {
    name = "info"
}
vPower = Visualizer {
    name = "power"
}
vFFT = Visualizer {
    name = "fft"
}
layout = Node (Verti, 0.5)
        (Leaf vInfo) (Node (Horiz, 0.5) (Leaf vPower) (Leaf vFFT))
layoutZ = (layout,[])

-- (w,h) -> ...
-- critical issue: we need to iterate breadth first!
-- because we first need to fill w and h of the root node
-- also, we have to extend window to have w and h...
-- options
-- 1. rewrite traverseParent to breadth first
-- 2. rewrite traverseParent to use Zipper instead of Tree (path to root tree would suffice)
-- 3. alter the function to take in a list of windows.
--    then always get some way to turn a zipper into a list of passed inner nodes
fillVisDims :: (Int, Int) -> Tree Visualizer Window -> Tree Visualizer Window -> Tree Visualizer Window
fillVisDims (w,h) (Node x _ _) (Leaf y) = Leaf y

fillVisDims2 :: (Int, Int) -> Zipper Visualizer Window -> Tree Visualizer Window
fillVisDims2 (w,h) (Leaf y, bs) = Leaf (updateVis y) 
    where
        ws = parents (Leaf y, bs)
        ps = unwords $ map (\w -> (show $ fst w)) ws
        updateVis x = x { name = ps }



-- make calculations on data possible
bytesToFloats :: BS.ByteString -> V.Vector Float
bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

toDouble :: V.Vector Float -> V.Vector Double
toDouble vec = V.map (\x -> float2Double x) vec

readSample :: Int -> Simple -> IO ByteString 
readSample ssize s = simpleReadRaw s ssize :: IO ByteString 

vecAbs :: V.Vector Float -> V.Vector Float
vecAbs vec = V.map (\v -> abs v) vec 


-- helper function to print a [Float]
-- for printing IO [Float]
-- is this needed?
-- or can I somehow compose show with putStrLn?
putStrLnFloat :: ByteString -> IO ()
putStrLnFloat bytes = do
    System.IO.putStrLn $ show $ V.maximum $ vecAbs $ bytesToFloats bytes

-- todo make a record for this...
-- (charsEmpty, charsFilled, charsFade) maxBarLen barLen
bar :: (Char, Char) -> String -> Int -> Int -> String
bar chs chFa mbarlen n | n < 3  = (take n chFa) ++ (replicate (mbarlen-n) $ fst chs)
      | n >= 3 && n <= mbarlen = filled ++ chFa ++ (replicate (maxBarLen-n) $ fst chs)
      | n > mbarlen = replicate maxBarLen $ snd chs
    where
        chF = reverse $ take n $ reverse chFa
        filled = replicate (n-3) $ snd chs



--
a = ' '
b = '#'
c = "|+-"
d = maxBarLen
barApplied = bar (a, b) c d 

strBar :: Int -> [String]
strBar n = map (\c -> [c]) (barApplied n)

-- value, maximum
vbar :: Float -> Float -> IO ()
vbar val maxi = putStrLn $ barApplied $ displayable val maxi




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


-- generic function applying the fft?
fft :: Plan Double (Complex Double) -> V.Vector Double -> V.Vector (Complex Double)
fft plan floats = execute plan floats

sq :: V.Vector (Complex Double) -> V.Vector Double
sq v = V.map magnitude v

fftAudio :: Husky -> ByteString -> Audio 
fftAudio h bs = aud
    where
        sample = toDouble $ bytesToFloats bs
        fftsample = fft fftPlan sample
        fftsqsample = sq fftsample
        aud = Audio {
            audioSample = sample,
            audioVolume = 0, -- todo fix this
            audioFFT = fftsample,
            audioFFTSq = fftsqsample
        }



volume :: ByteString -> IO ()
volume bs = vbar fmax volMaxChars 
    where
        fmax = V.maximum $ vecAbs $ bytesToFloats bs

-- old
maxfft = 40
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
            (barApplied val) ++ "\n" ++
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


-- takes in the squared fft values 
displayFFT :: Husky -> V.Vector Double -> Image 
displayFFT h vec = do
    foldt (\a b -> a <|> b) (head images) (tail images)
    where
        bins = binsToTake
        wdh = bins * barWidth
        slice = V.take bins vec
        lslice = V.toList slice
        images = map (\val -> valToImage val) lslice


centerRect :: (Int, Int) -> Image -> Image
centerRect (w, h) img = translate tx ty img
    where
        tx = fst $ (w - (binsToTake * barWidth)) `divMod` 2 
        ty = fst $ (h - maxBarLen) `divMod` 2


-- animate :: Vty -> ByteString -> IO ()
-- animate vty bs = do
--     region <- displayBounds $ outputIface vty
--     update vty $ picForImage (centerRect region (vertJoin fftoutput qline))
--     where
--         floats = bytesToFloats bs
--         fftoutput = volumefft3 floats
--         c = (binsToTake*barWidth)
--         qline = mline c '#'

displayAll :: Husky -> IO ()
displayAll h = do
    region <- displayBounds $ outputIface $ vtyInstance h
    update vty $ picForImage cropped 
    where
        -- img = visBox h info
        ffts = audioFFTSq (audio h)
        img = displayFFT h ffts
        wi = window_width h
        he = window_height h
        cropped = crop wi he img
        vty = vtyInstance h



handleIOEvent :: IO Event -> IO ()
handleIOEvent x = do
    return () 

-- loop forever reading audio :)
-- capture :: MVar Event -> Vty -> Simple -> Float -> IO ()
-- capture ioBox vty s prevVol = do
--     -- (3) get terminal size 
--     -- (2) check for MVar 
--     -- ev :: Maybe (IO Event)
--     ev <- tryTakeMVar ioBox
--     case ev of 
--         Nothing -> do
--             -- handleIOEvent x
--             (readSample defaultBufferchunk s) >>= (animate vty)
--             capture ioBox vty s prevVol
--         Just x -> do
--             -- free and return
--             if shouldAbort x 
--                 then gracefulShutdown vty s
--                 else capture ioBox vty s prevVol

gracefulShutdown :: Vty -> Simple -> IO ()
gracefulShutdown vty s = do
    shutdown vty
    simpleFree s


-- pattern match for Ctrl-C or q
shouldAbort :: Event -> Bool
shouldAbort ev = case ev of 
    (EvKey (KChar 'c') [MCtrl]) -> True
    (EvKey (KChar 'q') _) -> True
    _ -> False


watchForIOEvents :: Husky -> IO ()
watchForIOEvents h = do
    ev <- nextEvent vty
    putStrLn $ show ev

    -- send over to other thread
    putMVar qiobox $ ev 

    if shouldAbort ev
        then exitWith ExitSuccess
        else watchForIOEvents h 
    where
        qiobox = ioBox h
        vty = vtyInstance h


spin :: Husky -> IO ()
spin h = do
    ev <- tryTakeMVar $ ioBox h
    case ev of 
        Nothing -> do
            -- handleIOEvent x

            -- vty bounds
            region <- displayBounds $ outputIface vty

            -- audio
            sample <- readSample defaultBufferchunk s
            let nAudio = fftAudio h sample

            -- update Husky object
            let hnew = ((updateAudio nAudio) . (updateVty region)) h 

            -- show
            displayAll hnew

            -- recurse
            spin hnew 

        Just x -> do
            -- free and return
            if shouldAbort x 
                then gracefulShutdown vty s 

                -- recurse
                else spin h 
    where
        vty = (vtyInstance h)
        s = (recSimple h)
        updateAudio x y = y { audio = x }
        updateVty x y = y { window_width = fst x, window_height = snd x}



main = do

    -- communication between UI and recorder thread
    qiobox <- newEmptyMVar

    -- simple pulse object
    let tit = title huskyDefault
    let desc = description huskyDefault
    let sr = samplerate huskyDefault
    let ba = recBufattr huskyDefault
    s <- simpleNew Nothing tit Record Nothing desc 
      (SampleSpec (F32 LittleEndian) sr 1) Nothing ba

    -- create vty
    vty <- mkVty defaultConfig

    -- update Husky initial object
    let updateSimple x = x { recSimple = s }
    let updateVty x = x { vtyInstance = vty }
    let updateIoBox x = x { ioBox = qiobox }
    let newHusky = (updateIoBox . updateVty . updateSimple) huskyDefault

    -- spin
    forkIO $ spin $ newHusky

    -- (1) start vty watcher thread
    -- forkIO $ capture ioBox vty s 1.0

    -- watch for IO Events
    watchForIOEvents newHusky 


