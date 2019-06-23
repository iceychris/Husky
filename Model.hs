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


-- type aliases
type ZWV = Zipper Window Visualizer
type WinRes = (Window, Resolution)
type VisRes = (Visualizer, Resolution)

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
    audioMemFFT :: V.Vector Double,
    audioFFT :: V.Vector (Complex Double),
    audioFFTSqHistory :: [V.Vector Double],
    audioFFTAdd :: V.Vector Double
}

-- is this modeled adequately?
-- try omitting Husky in the type list
-- instead, maybe just pass the audio data?
data Visualizer = Visualizer {
    vis_name :: String,
    visualize :: Visualizer -> Husky -> Image
}
instance Show Visualizer where show v = "Vis(" <> vis_name v <> ")"

data Orient =  Horiz | Verti deriving (Eq, Show, Read)
data Resolution = Resolution {
    width :: Int,
    height :: Int
} deriving (Eq, Show, Read)

-- inner nodes
-- are Windows 
data Window = Window {
    orient :: Orient,
    percentage :: Double
} deriving (Eq, Show, Read)

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

-- inherit in a root-to-leaf fashion
--  f_branches -> f_leaves -> default_branch -> tree
-- where f_branches args:
--  (last result of f_branches -> branch value)
-- and f_leaves args:
--  (last result of f_branches -> leaf value)
inheritT :: (c -> a -> c) -> (c -> b -> d) -> c -> BiTree a b -> BiTree c d
inheritT f g i t = case t of
    Leaf x -> Leaf (g i x)
    Branch y l r -> Branch new (go l) (go r)
        where
            new = f i y
            go = inheritT f g new

-- annotate a tree with its height
annoHeight :: a -> BiTree a b -> BiTree (a, Int) (b, Int)
annoHeight defa = inheritT f f (defa, 0 :: Int) 
    where
        f cc bb = (bb, snd cc + 1)


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
