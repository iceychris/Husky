{-# language DeriveFunctor, DeriveTraversable, DeriveFoldable, TypeFamilies  #-}

module Model where

-- builtins
import Data.Maybe
import Data.Int
import Data.Complex
import GHC.Float
import Data.Typeable
import Data.Bifunctor
import Data.Functor.Foldable
import Control.Concurrent.MVar (MVar)

-- 3rd party
import qualified Data.Vector.Storable as V
import Sound.Pulse.Simple
import Numeric.FFT.Vector.Plan
import Numeric.FFT.Vector.Unitary
import qualified Numeric.FFT.Vector.Invertible as I

import Graphics.Vty

-- local
import Util


type ZWV = Zipper Window Visualizer

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
    window_layout :: ZWV,
    
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
    audioSampleFiltered :: V.Vector Double,
    audioVolume :: Int,
    audioFFT :: V.Vector (Complex Double),
    audioFFTSqHistory :: [V.Vector Double],
    audioFFTAdd :: V.Vector Double
}

weightedAverageSq :: [Double] -> Audio -> V.Vector Double
weightedAverageSq coef aud =
    if not lengthesMatch then error "lengths of coefficients and history do not match"
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
--  ~0.8sec for (V.length vec == 4_000_000) vecs
addVecs :: V.Vector Double -> V.Vector Double -> V.Vector Double 
addVecs a b = V.fromList $ zipWith (+) la lb
    where
        la = V.toList a
        lb = V.toList b


-- is this modeled adequately?
-- try omitting Husky in the type list
-- instead, maybe just pass the audio data?
data Visualizer = Visualizer {
    vis_name :: String,
    vis_width :: Int,
    vis_height :: Int,
    visualize :: Visualizer -> Husky -> Audio -> Image
}
instance Show Visualizer where show v = "Vis(" ++ show (vis_width v) ++ ", " ++ show (vis_height v) ++ ")"


data Orient =  Horiz | Verti deriving (Eq, Show)

-- inner nodes
-- are Windows 
data Window = Window {
    orient :: Orient,
    percentage :: Double,
    win_width :: Int,
    win_height :: Int
} deriving (Eq, Show)

-- leaves 
-- are Visualizers


------------
-- BiTree implementation
-- taken from https://stackoverflow.com/questions/41524960/recursion-schemes-using-fix-on-a-data-type-thats-already-a-functor
------------

data BiTree b l =
    Branch b (BiTree b l) (BiTree b l)
    | Leaf l
    deriving (Show, Typeable, Functor, Traversable, Foldable)

instance Bifunctor BiTree where
  bimap _ g (Leaf x) = Leaf (g x)
  bimap f g (Branch b l r) = Branch (f b) (bimap f g l) (bimap f g r)

data BiTreeF b l r =
  BranchF b r r
    | LeafF l
    deriving (Show, Functor, Typeable)

type instance Base (BiTree a b) = BiTreeF a b
instance Recursive (BiTree a b) where
  project (Leaf x) = LeafF x
  project (Branch s l r) = BranchF s l r

instance Corecursive (BiTree a b) where
  embed (BranchF sp x xs) = Branch sp x xs
  embed (LeafF x) = Leaf x


------------
-- Zippers
------------

data Crumb b l = LeftCrumb b (BiTree b l) | RightCrumb b (BiTree b l) deriving (Show, Functor, Foldable)  
type Breadcrumbs b l = [Crumb b l] 
type Zipper b l = (BiTree b l, Breadcrumbs b l)

goLeft :: Zipper b l -> Maybe (Zipper b l)  
goLeft (Branch x ll rr, bs) = Just (ll, LeftCrumb x rr:bs)  
goLeft (Leaf y, _) = Nothing  
  
goRight :: Zipper b l -> Maybe (Zipper b l)  
goRight (Branch x ll rr, bs) = Just (rr, RightCrumb x ll:bs)  
goRight (Leaf y, _) = Nothing  

isLeaf :: Maybe (Zipper b l) -> Bool
isLeaf (Just (Leaf _, _)) = True
isLeaf _ = False

goLeftUnsafe :: Zipper b l -> Zipper b l 
goLeftUnsafe (Branch x ll rr, bs) = (ll, LeftCrumb x rr:bs)
goLeftUnsafe (t, bs) = (t, bs)  
  
goRightUnsafe :: Zipper b l -> Zipper b l
goRightUnsafe (Branch x ll rr, bs) = (rr, RightCrumb x ll:bs)
goRightUnsafe (t, bs) = (t, bs)  

goUpUnsafe :: Zipper b l -> Zipper b l  
goUpUnsafe (t, LeftCrumb x r:bs) = (Branch x t r, bs)
goUpUnsafe (t, RightCrumb x l:bs) = (Branch x l t, bs)

parents :: Zipper b l -> [b]
parents (_, []) = []
parents (_, [LeftCrumb x t]) = [x]
parents (_, [RightCrumb x t]) = [x]
parents (t, (LeftCrumb x t2):cs)  = [x] ++ parents (t, cs) 
parents (t, (RightCrumb x t2):cs) = [x] ++ parents (t, cs) 

parentsMaybe :: Maybe (Zipper b l) -> [b]
parentsMaybe Nothing = []
parentsMaybe (Just z) = parents z 

-- refactor this into inherit
traverseContextBF :: Zipper b l -> (Zipper b l -> Zipper b l) -> Zipper b l
traverseContextBF (Leaf a, bs) f = f (Leaf a, bs)
traverseContextBF (Branch x l r, bs) f = rec zApplied 
    where 
        z = (Branch x l r, bs)
        zApplied = f z
        rec (Leaf a, bsr) = error "rec (Leaf a,...) called. this should not happen. Audit your zip -> zip function"
        rec (Branch y ll rr, bsr) =
            (Branch y
                (fst (traverseContextBF (goLeftUnsafe (mtree, bsr)) f))
                (fst (traverseContextBF (goRightUnsafe (mtree, bsr)) f))
            , bsr)
            where
                mtree = (Branch y ll rr)
