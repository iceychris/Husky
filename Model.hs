{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module Model where

-- builtins
import Data.Maybe
import Data.Int
import Data.Complex
import GHC.Float
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, tryTakeMVar, putMVar, MVar)

-- 3rd party
import qualified Data.Vector.Storable as V
import Sound.Pulse.Simple
import Numeric.FFT.Vector.Plan
import Numeric.FFT.Vector.Unitary
import qualified Numeric.FFT.Vector.Invertible as I

import Graphics.Vty

-- local
import Util


data Husky = Husky {
    title :: String,
    description :: String,

    samplerate :: Int,
    bufferchunk :: Int,

    fftInput :: Int,
    -- fftTransform :: Transform,
    -- fftPlan :: Plan,

    vtyInstance :: Vty,
    window_width :: Int,
    window_height :: Int,
    
    charsEmpty :: Char,
    charsFilled :: Char,
    charsFade :: String,
    
    recBufattr :: Maybe BufferAttr,
    recSimple :: Simple,

    audio :: Audio,

    ioBox :: MVar Event
}

data Audio = Audio {
    audioSample :: V.Vector Double,
    audioVolume :: Int,
    audioFFT :: V.Vector (Complex Double),
    audioFFTSqHistory :: [V.Vector Double],
    audioFFTAdd :: V.Vector Double
}

weightedAverageSq :: [Double] -> Audio -> V.Vector Double
weightedAverageSq coef aud =
    if not lengthesMatch then error "lengthes of coefficients and history do not match"
    else if not coefAddUpToOne then error "coefficients do not add up to 1"
    else ret 
    where
        hist = audioFFTSqHistory aud
        lengthesMatch = length coef == length hist
        coefAddUpToOne = roughlyEqual 0.01 (sum coef) 1.0

        -- zip coef and history together
        zipped = zip coef hist 

        -- multiply coef with corresponding vector
        multiplied = map (\x -> (V.map (\elem -> (fst x) * elem)) (snd x)) zipped
        -- add these vectors up to one vector
        accu = V.fromList (replicate (V.length $ head $ audioFFTSqHistory aud) 0.0) :: V.Vector Double
        ret = foldt (\acc val -> addVecs acc val) accu multiplied 


-- fix this shitty stuff...
-- TODO implement addition here
-- TODO VERY INEFFICIENT
--  ~0.8sec for 4_000_000 long vecs
addVecs :: V.Vector Double -> V.Vector Double -> V.Vector Double 
addVecs a b = V.fromList $ zipWith (+) la lb
    where
        la = V.toList a
        lb = V.toList b


-- is this modeled adequately?
-- try omitting Husky in the type list
-- instead, maybe just pass the audio data?
data Visualizer = Visualizer {
    name :: String,
    vis_width :: Int,
    vis_height :: Int,
    visualize :: Visualizer -> Audio -> Image
}
instance Show Visualizer where show vis = name vis 


data Orient =  Horiz | Verti deriving (Show)
type Percentage = Double

-- inner nodes
-- are Windows 
type Window = (Orient, Percentage)

-- leaves 
-- are Visualizers


-- can we use this as a basic datastructure for our windows?
-- it has some representations were multiple configs are possible
-- data Tree a = Empty | Node a (Tree a) (Tree a) | Leaf a deriving (Show)  

-- instead one could just use this with maybe data...
data Tree a b = Leaf a | Node b (Tree a b) (Tree a b) deriving (Eq, Show, Functor, Foldable)
data Crumb a b = LeftCrumb b (Tree a b) | RightCrumb b (Tree a b) deriving (Show, Functor, Foldable)  
type Breadcrumbs a b = [Crumb a b] 
type Zipper a b = (Tree a b, Breadcrumbs a b)
type ZipVisWin = Zipper Visualizer Window

goLeft :: Zipper a b -> Maybe (Zipper a b)  
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)  
goLeft (Leaf y, _) = Nothing  
  
goRight :: Zipper a b -> Maybe (Zipper a b)  
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)  
goRight (Leaf y, _) = Nothing  

isLeaf :: Maybe (Zipper a b) -> Bool
isLeaf (Just (Leaf _, _)) = True
isLeaf _ = False

goLeftUnsafe :: Zipper a b -> Zipper a b 
goLeftUnsafe (Node x l r, bs) = (l, LeftCrumb x r:bs)
goLeftUnsafe (t, bs) = (t, bs)  
  
goRightUnsafe :: Zipper a b -> Zipper a b
goRightUnsafe (Node x l r, bs) = (r, RightCrumb x l:bs)
goRightUnsafe (t, bs) = (t, bs)  

goUpUnsafe :: Zipper a b -> Zipper a b  
goUpUnsafe (t, LeftCrumb x r:bs) = (Node x t r, bs)
goUpUnsafe (t, RightCrumb x l:bs) = (Node x l t, bs)



parents :: Zipper a b -> [b]
parents (_, []) = []
parents (_, [LeftCrumb x t]) = [x]
parents (_, [RightCrumb x t]) = [x]
parents (t, (LeftCrumb x t2):cs)  = [x] ++ parents (t, cs) 
parents (t, (RightCrumb x t2):cs) = [x] ++ parents (t, cs) 

parentsMaybe :: Maybe (Zipper a b) -> [b]
parentsMaybe Nothing = []
parentsMaybe (Just z) = parents z 

traverseContextBF :: Zipper a b -> (Zipper a b -> Zipper a b) -> Zipper a b
traverseContextBF (Leaf a, bs) f = f (Leaf a, bs)
traverseContextBF (Node x l r, bs) f = rec zApplied 
    where 
        z = (Node x l r, bs)
        zApplied = f z
        rec (Leaf a, bsr) = error "rec (Leaf a,...) called. this should not happen. Audit your zip -> zip function"
        rec (Node y ll rr, bsr) =
            (Node y
                (fst (traverseContextBF (goLeftUnsafe (mtree, bsr)) f))
                (fst (traverseContextBF (goRightUnsafe (mtree, bsr)) f))
            , bsr)
            where
                mtree = (Node y ll rr)

-- testing
-- demo data
t = Node "parent"
    (Node "l1"
        (Leaf ["L_ll1"])
        (Leaf ["L_lr1"]))
    (Node "r1"
        (Leaf ["L_rl1"])
        (Node "rr1" (Leaf ["L_rrl1"]) (Leaf ["L_rrr1"])))
zipa = (t, [])
mfoc = return zipa >>= goLeft >>= goRight

exf :: Zipper [[Char]] [Char] -> Zipper [[Char]] [Char]
exf (Leaf x, bs) = (Leaf ((parents z) ++ x), bs)
    where 
        z = (Leaf x, bs)
exf (Node x l r, bs) = (Node ((unwords $ parents z) ++ x) l r, bs) 
    where 
        z = (Node x l r, bs)


-- define:
    -- OK zipper
    -- OK goUp, goLeft, goRight, isLeaf
    -- method to locate visualizer based on its name
    -- render :: Zipper a b -> Image

