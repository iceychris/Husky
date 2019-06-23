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
import Data.Functor
import Debug.Trace
import GHC.Float
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, putMVar, MVar)

-- 3rd party
import Data.ByteString.Internal as BS
import Data.Time.Clock.POSIX
import qualified Data.Vector.Storable as V
import Sound.Pulse.Simple
import Numeric.FFT.Vector.Plan
import Numeric.FFT.Vector.Unitary
-- import qualified Numeric.FFT.Vector.Invertible as I
import qualified Numeric.FFT.Vector.Unitary as I
import Data.Typeable
import Data.Bifunctor
import Data.Functor.Foldable

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
-- OK remove charsFade when input value < 3
-- OK fix output dimensions
    -- OK (3) get terminal size 

-- fft
    -- OK save the last fft (list?) and average with the current one to show image
    -- fix interestedFFTPart stuff and downsampling to avoid taking 0 values
    -- implement log scaling
    -- create little indicators sweeping from top to bottom for peaks
    -- add vertical mode
-- figure out tree stuff...
-- criterion measuring
-- or profiling
-- look for features in colorchord 2

-- refactoring
    -- OK delete old commented out code
    -- move real visualizers into Visualizers.hs
    -- create Config.hs and move all initial structures


qgracefulShutdown :: Husky -> IO ()
qgracefulShutdown h = gracefulShutdown (vtyInstance h) (recSimple h)

gracefulShutdown :: Vty -> Simple -> IO ()
gracefulShutdown vty s = do
    putStrLn "gracefully shutting down Vty and Simple..."
    sequence_ [shutdown vty, simpleFree s] 


-- move these
volMaxChars = 300.0 -- 0.8 -- scaling
barWidth = 2
binsToTake = 100
maxBarLen = 15 -- height
lossPerFrame = 0.00001

-- fft options
historySize = 7
waCoefs = [0.5, 0.15, 0.15, 0.05, 0.05, 0.05, 0.05]
interestedFFTPart = 1.0

-- initial values
defaultBufferchunk = 4096 -- 1024
fftInputs = (fst (defaultBufferchunk `divMod` 4))
fftTransform = I.dftR2C
fftPlan = plan fftTransform fftInputs
demoVec = V.fromList $ replicate defaultBufferchunk 0.0

defaultAudio = Audio {
    audioVolume = 0,
    audioFFT    = V.fromList $ replicate defaultBufferchunk 0.0,
    audioMemFFT = demoVec,
    audioSample = demoVec,
    audioFFTSqHistory = replicate historySize demoVec
}
huskyDefault = Husky {
    title       = "Husky",
    description = "Audio Visualizer",

    samplerate       = 44100,
    bufferchunk      = defaultBufferchunk,

    fftInput         = fftInputs,

    window_width     = 1400,
    window_height    = 900,
    window_layout    = defaultLayout,

    charsEmpty       = ' ',
    charsFilled      = '█',
    charsFade        = "▓▒░",

    recBufattr       = Just $ BufferAttr Nothing Nothing Nothing Nothing $ Just $ samplerate huskyDefault,

    audio            = defaultAudio
}

vFFT = Visualizer {
    vis_name = "fft",
    visualize = visFFT
}
ver = Window {
    orient = Verti,
    percentage = 0.5
}
hor = Window {
    orient = Horiz,
    percentage = 0.9
}
defaultLayout = (Branch ver 
    (Branch hor
        (Leaf vHInfo)
        (Leaf vVInfo))
    (Branch hor
        (Leaf vFFT)
        (Leaf vPower)) , [])

fixDimsBranches :: (Window, Resolution) -> Window -> (Window, Resolution)
fixDimsBranches (pw, pr) w = (w, rnew)
    where
        parent = pw
        pcent = percentage parent
        winw = width pr
        winh = height pr
        nw Horiz = winw
        nw Verti = floor ((fromIntegral winw) * pcent) 
        nh Horiz = floor ((fromIntegral winh) * pcent)
        nh Verti = winh 
        rnew = Resolution {
            width = nw $ orient parent,
            height = nh $ orient parent
        }

fixDimsLeaves :: (Window, Resolution) -> Visualizer -> (Visualizer, Resolution)
fixDimsLeaves (pw, pr) v = (v, rnew)
    where
        parent = pw
        pcent = percentage parent
        winw = width pr
        winh = height pr
        nw Horiz = winw
        nw Verti = floor ((fromIntegral winw) * pcent) 
        nh Horiz = floor ((fromIntegral winh) * pcent)
        nh Verti = winh 
        rnew = Resolution {
            width = nw $ orient parent,
            height = nh $ orient parent
        }

render :: Husky -> Image
render husky = cata alg calculatedT where
    mwin = Window {
        orient = Verti,
        percentage = 1.0
    }
    res = Resolution {
        width = window_width husky,
        height = window_height husky
    }
    layout = window_layout husky
    calculatedT = inheritT fixDimsBranches fixDimsLeaves (mwin, res) $ fst layout

    alg :: BiTreeF WinRes VisRes Image -> Image
    alg (LeafF v) = execAndResize husky v 
    alg (BranchF w li ri)
        | orient (fst w) == Verti = li <|> ri
        | otherwise               = li <-> ri

execAndResize :: Husky -> VisRes -> Image
execAndResize husky (v, res) = resized where
    w = width res
    h = height res
    img = (visualize v) v husky
    resized = resize w h img


-- make calculations on data possible
bytesToFloats :: BS.ByteString -> V.Vector Float
bytesToFloats = V.unsafeCast . aux . BS.toForeignPtr
  where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

toDouble :: V.Vector Float -> V.Vector Double
toDouble = V.map float2Double

readSample :: Int -> Simple -> IO ByteString 
readSample ssize s = simpleReadRaw s ssize :: IO ByteString 

vecAbs :: V.Vector Float -> V.Vector Float
vecAbs = V.map abs 


-- helper function to print a [Float]
-- for printing IO [Float]
-- is this needed?
-- or can I somehow compose show with putStrLn?
putStrLnFloat :: ByteString -> IO ()
putStrLnFloat bytes = print $
    V.maximum $ vecAbs $ bytesToFloats bytes

strBar :: Int -> [String]
strBar n = map (: []) (barApplied n)

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
imbar n wwidth = horizCat $ replicate wwidth columns 
    where
        onebar = reverse (strBar n)
        columns = vertCat (map (string defAttr) onebar)


-- generic function applying the fft?
fft :: Plan Double (Complex Double) -> V.Vector Double -> V.Vector (Complex Double)
fft = execute

sq :: V.Vector (Complex Double) -> V.Vector Double
sq = V.map magnitude

addy :: V.Vector (Complex Double) -> V.Vector Double
addy = V.map (\c -> realPart c + imagPart c)



-- TODO: fix audio
-- TODO: fix addy vs sq
-- TODO: fix volume
fftAudio :: Husky -> ByteString -> Audio 
fftAudio h bs = aud2
    where
        sample = toDouble $ bytesToFloats bs
        fftsample = fft fftPlan sample
        fftsqsample = sq fftsample
        fftaddsample = addy fftsample
        prevAud = audio h
        prevHist = audioFFTSqHistory prevAud
        aud = Audio {
            audioSample = sample,
            audioVolume = 0,
            audioFFT = fftsample,
            audioMemFFT = fftaddsample,
            audioFFTAdd = fftaddsample,
            audioFFTSqHistory = fftsqsample:(init prevHist) 
        }
        aud2 = (applyMemory aud) { audioFFTSqHistory = (audioMemFFT aud):(init (audioFFTSqHistory aud))}

applyMemory :: Audio -> Audio
applyMemory aud = aud2
    where
        a = head $ audioFFTSqHistory aud
        b = audioFFTSqHistory aud !! 1
        memFFT = V.fromList $ fmap magic $ zip (V.toList a) (V.toList b)
        magic p@(aa, bb) = if aa > bb then aa else bb - lossPerFrame
        updateAudioFFT x = aud { audioMemFFT = x }
        aud2 = updateAudioFFT memFFT 

rescaleTodBForDisplay :: V.Vector Double -> V.Vector Double
rescaleTodBForDisplay = V.map fun
    where
        fun x = if x <= 1.0 then x else 1.0 * ((log x) / (log 10)) ** 2

positive :: V.Vector Double -> V.Vector Double
positive = V.map (\x -> if x >= 0.0 then x else 0.0)

maxNormalize :: V.Vector Double -> V.Vector Double
maxNormalize v = if _max >= 0.01 then V.map (\x -> x / _max) v else v 
    where
        _max = V.maximum v

volume :: ByteString -> IO ()
volume bs = vbar fmax volMaxChars 
    where
        fmax = V.maximum $ vecAbs $ bytesToFloats bs

valToImage :: Double -> Image
valToImage val = imbar (displayable (double2Float val) volMaxChars) barWidth

mline :: Int -> Char -> Image
mline wdh ch = string defAttr (replicate wdh ch)

-- perform (fake) downsampling to
-- reduce the length to nL
downsample :: Int -> V.Vector Double -> V.Vector Double
downsample nL vec = vecsSums 
    where 
        vecLength = V.length vec
        calc = (vecLength `div` nL)
        oneBin = floor (fromIntegral calc)
        vecs = decimate oneBin vec 
        vecsSums = V.fromList (map V.maximum vecs)

decimate :: Int -> V.Vector Double -> [V.Vector Double]
decimate binL vec = case l of
    0 -> []
    _ -> front:decimate binL back
    where 
        l = V.length vec
        front = V.take binL vec
        back = V.drop binL vec

visFFT :: Visualizer -> Husky -> Image
visFFT _ h = {- trace ("vec is " <> show lslice) -} img
    where
    aud = audio h
    vec = (maxNormalize . positive) $ audioMemFFT aud
    -- vec = (maxNormalize . rescaleTodBForDisplay . maxNormalize . positive) $ audioMemFFT aud
    -- vec = audioFFTAdd aud
    lslice = V.toList vec
    images = map valToImage lslice
    img = foldt (<|>) (head images) (tail images)

centerRect :: (Int, Int) -> Image -> Image
centerRect (w, h) = translate tx ty
    where
        tx = fst $ (w - (binsToTake * barWidth)) `divMod` 2 
        ty = fst $ (h - maxBarLen) `divMod` 2

displayAll :: Husky -> IO ()
displayAll h = update vty $ picForImage (render h) 
    where
        vty = vtyInstance h

-- pattern match for Ctrl-C or q
shouldAbort :: Event -> Bool
shouldAbort ev = case ev of 
    (EvKey (KChar 'c') [MCtrl]) -> True
    (EvKey (KChar 'q') _) -> True
    _ -> False

watchForIOEvents :: Husky -> IO ()
watchForIOEvents h = do

    -- gather the next event
    ev <- nextEvent vty

    -- send over to other thread
    putMVar qiobox $ ev 

    if shouldAbort ev
        then exitSuccess
        else watchForIOEvents h 
    where
        qiobox = ioBox h
        vty = vtyInstance h


spin :: Husky -> IO ()
spin h = do
    ev <- tryTakeMVar $ ioBox h
    case ev of 
        Nothing -> do

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

        Just iev ->
            -- free and return
            if shouldAbort iev 
                then gracefulShutdown vty s 

            -- recurse
            else spin h
    where
        vty = (vtyInstance h)
        s = (recSimple h)
        updateAudio x y = y { audio = x }
        updateVty x y = y { window_width = fst x, window_height = snd x}


main :: IO ()
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
    _ <- forkIO $ spin newHusky

    -- watch for IO Events
    watchForIOEvents newHusky 


