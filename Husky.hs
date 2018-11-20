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



qgracefulShutdown :: Husky -> IO ()
qgracefulShutdown h = do
    putStrLn "qgracefulShutdow not implemented"
    --(vtyInstance h) >>= shutdown
    --(recSimple h) >>= simpleFree



-- move these
volMaxChars = 30.0 -- 0.8 -- scaling
barWidth = 1
binsToTake = 100
maxBarLen = 15 -- height

-- fft options
historySize = 7
waCoefs = [0.5, 0.15, 0.15, 0.05, 0.05, 0.05, 0.05]
interestedFFTPart = 0.7

-- initial values
defaultBufferchunk = 2048 -- 1024
fftInputs = (fst (defaultBufferchunk `divMod` 4))
fftTransform = I.dftR2C
fftPlan = plan fftTransform fftInputs
demoVec = V.fromList $ replicate defaultBufferchunk 0.0

defaultAudio = Audio {
    audioVolume = 0,
    audioFFT    = V.fromList $ replicate defaultBufferchunk 0.0,
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

vInfo = Visualizer {
    vis_name = "info",
    visualize = visDummy,
    vis_width = -1,
    vis_height = -1
}
vPower = Visualizer {
    vis_name = "power",
    visualize = visDummy,
    vis_width = -1,
    vis_height = -1
}
vFFT = Visualizer {
    vis_name = "fft",
    visualize = visFFT,
    vis_width = -1,
    vis_height = -1
}
ver = Window {
    orient = Verti,
    percentage = 0.5,
    win_width = 0,
    win_height = 0
}
hor = Window {
    orient = Horiz,
    percentage = 0.6,
    win_width = 0,
    win_height = 0
}
defaultLayout = (Branch ver 
        (Leaf vInfo) (Branch hor (Leaf vPower) (Leaf vFFT)), [])
-- defaultLayout = (Leaf vInfo, [])
-- defaultLayout = (Branch ver (Leaf vInfo) (Leaf vPower), [])


-- (w,h) -> ...
-- TODO make this function shorter...
fillVisDims :: (Int, Int) -> ZWV -> ZWV 
fillVisDims (w,h) (Branch x l r, []) = z
    where
        update x w h = x { win_width = w, win_height = h }
        z = (Branch (update x w h) l r, [])
fillVisDims (w,h) (Leaf y, []) = z
    where
        update x w h = x { vis_width = w, vis_height = h }
        z = (Leaf (update y w h), [])
fillVisDims (w,h) (Branch x l r, bs) = (Branch (update x nww nhh) l r, bs) 
    where 
        parent = head $ parents (Branch x l r, bs)
        pcent = percentage parent
        winw = win_width parent
        winh = win_height parent
        nw Horiz = winw
        nw Verti = floor ((fromIntegral winw) * pcent) 
        nh Horiz = floor ((fromIntegral winh) * pcent)
        nh Verti = winh 
        nww = nw $ orient parent
        nhh = nh $ orient parent
        update x w h = x { win_width = w, win_height = h }
fillVisDims (w,h) (Leaf y, bs) = (Leaf (update y nww nhh), bs)
    where 
        parent = head $ parents (Leaf y, bs)
        pcent = percentage parent
        winw = win_width parent
        winh = win_height parent
        nw Horiz = winw
        nw Verti = floor ((fromIntegral winw) * pcent) 
        nh Horiz = floor ((fromIntegral winh) * pcent)
        nh Verti = winh 
        nww = nw $ orient parent
        nhh = nh $ orient parent
        update x w h = x { vis_width = w, vis_height = h }

render :: Husky -> Image
render husky = cata alg calculatedT where
    aud = audio husky
    w = window_width husky
    h = window_height husky
    layout = window_layout husky
    calculatedT = fst (traverseContextBF defaultLayout $ fillVisDims (w,h))

    alg :: (BiTreeF Window Visualizer Image) -> Image
    alg (LeafF v) = execAndResize husky v 
    alg (BranchF w li ri)
        | (orient w) == Verti = li <|> ri
        | otherwise           = li <-> ri

execAndResize :: Husky -> Visualizer -> Image
execAndResize husky v = resized where
    w = (vis_width v)
    h = (vis_height v)
    img = (visualize v) v husky (audio husky)
    resized = resize (w) (h) img


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
b = '█'
c = "▓▒░"
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

addy :: V.Vector (Complex Double) -> V.Vector Double
addy v = V.map (\c -> realPart c + imagPart c) v



-- TODO: fix audio
-- TODO: fix addy vs sq
fftAudio :: Husky -> ByteString -> Audio 
fftAudio h bs = aud
    where
        sample = toDouble $ bytesToFloats bs
        fftsample = fft fftPlan sample
        fftsqsample = sq fftsample
        fftaddsample = addy fftsample
        prevAud = audio h
        prevHist = audioFFTSqHistory prevAud
        aud = Audio {
            audioSample = sample,
            audioVolume = 0, -- todo fix this
            audioFFT = fftsample,
            audioFFTAdd = fftaddsample,
            audioFFTSqHistory = [fftsqsample] ++ (init prevHist) 
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

-- perform (fake) downsampling to
-- reduce the length to nL
downsample :: Int -> V.Vector Double -> V.Vector Double
downsample nL vec = vecsSums 
    where 
        vecLength = V.length vec
        calc = (vecLength `div` nL)
        oneBin = floor (fromIntegral calc)
        vecs = decimate oneBin vec 
        vecsSums = V.fromList (map (\x -> V.maximum x) vecs)

decimate :: Int -> V.Vector Double -> [V.Vector Double]
decimate binL vec = case l of
    0 -> []
    _ -> [front] ++ (decimate binL back)
    where 
        l = V.length vec
        front = (V.take binL vec)
        back = (V.drop binL vec)

visFFT :: Visualizer -> Husky -> Audio -> Image
visFFT v h a = img where
    vec = weightedAverageSq waCoefs a 
    bins = (vis_width v) `div` barWidth
    wdh = bins * barWidth
    l = fromIntegral (V.length vec) :: Double
    interested = V.take (floor (l * interestedFFTPart)) vec
    slice = downsample bins interested
    lslice = V.toList slice
    images = map (\val -> valToImage val) lslice
    img = foldt (\a b -> a <|> b) (head images) (tail images)



-- takes in the squared fft values 
-- displayFFT :: Husky -> V.Vector Double -> Image 
-- displayFFT h vec = do
--     foldt (\a b -> a <|> b) (head images) (tail images)
--     where
--         bins = binsToTake
--         wdh = bins * barWidth
--         l = fromIntegral (V.length vec) :: Double
--         interested = V.take (floor (l * interestedFFTPart)) vec
--         slice = downsample bins interested
--         lslice = V.toList slice
--         images = map (\val -> valToImage val) lslice


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
    update vty $ picForImage (render h) 
    where
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
    -- putStrLn $ show ev

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


