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

-- local
import Graphics.Vty


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
    audioFFTSq :: V.Vector Double,
    audioFFTAdd :: V.Vector Double
}

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


-- ensure you pass bs=[] here (root of the tree)
-- function is
--   (parent, child) -> child
-- traverseParent :: Zipper a b -> (Tree a b -> Tree a b -> Tree a b) -> Zipper a b
-- 
-- -- make sure to go up the tree if not already
-- traverseParent (z, [x]) f = traverseParent (goUpUnsafe (z, [x])) f
-- traverseParent (z, bc:bcs) f  = traverseParent (goUpUnsafe (z, bc:bcs)) f
-- 
-- traverseParent (Empty, bs) fun = (Empty, bs)
-- traverseParent (Leaf a, bs) fun = (Leaf a, bs)
-- 
-- traverseParent (Node x (Leaf l) (Leaf r), bs) fun =
--     (Node x (fun (Node x Empty Empty) (Leaf l)) (fun (Node x Empty Empty) (Leaf r)), bs)
-- 
-- traverseParent (Node x (Leaf l) r, bs) fun =
--     (Node x (fun (Node x Empty Empty) (Leaf l)) (fst (traverseParent (r, bs) fun)), bs)
-- 
-- traverseParent (Node x l (Leaf r), bs) fun =
--     (Node x (fst (traverseParent (l, bs) fun)) (fun (Node x Empty Empty) (Leaf r)), bs)
-- 
-- traverseParent (Node x l r, bs) fun =
--     (Node x (fst (traverseParent (l, bs) fun)) (fst (traverseParent (r, bs) fun)), bs)
-- 
-- traverseParent (Node x Empty Empty, bs) fun = (Node x Empty Empty, bs)



-- inner zipper has Leaf node in focus
--
-- traverseParent2 :: Zipper a b -> (Zipper a b -> Tree a b) -> Zipper a b
-- 
-- -- make sure to go up the tree if not already
-- traverseParent2 (z, [x]) f = traverseParent2 (goUpUnsafe (z, [x])) f
-- traverseParent2 (z, bc:bcs) f  = traverseParent2 (goUpUnsafe (z, bc:bcs)) f
-- 
-- traverseParent2 (Node x (Leaf l) (Leaf r), bs) fun =
--     (Node x lapplied rapplied, bs)
--     where
--         t = Node x (Leaf l) (Leaf r)
--         lapplied = fun (goLeftUnsafe (t, bs))
--         rapplied = fun (goRightUnsafe (t, bs))
--         
-- traverseParent2 (Node x (Node y l1 r1) (Node z l2 r2), bs) fun =
--      
--     where
--         t = (Node x (Node y l1 r1) (Node y l1 r1)
--         tapplied = fun (t, bs)
--         la = goLeftUnsafe (tapplied, bs)
--         ra = goRightUnsafe (tapplied, bs)
    

-- 
-- -- we do not want these cases
-- traverseParent2 (Empty, bs) fun = (Empty, bs)
-- traverseParent2 (Leaf a, bs) fun = (Leaf a, bs)
-- 
-- traverseParent2 (Node x (Leaf l) (Leaf r), bs) fun =
--     (Node x (fun (Node x Empty Empty) (Leaf l)) (fun (Node x Empty Empty) (Leaf r)), bs)
-- 
-- traverseParent2 (Node x (Leaf l) r, bs) fun =
--     (Node x (fun (Node x Empty Empty) (Leaf l)) (fst (traverseParent2 (r, bs) fun)), bs)
-- 
-- traverseParent2 (Node x l (Leaf r), bs) fun =
--     (Node x (fst (traverseParent2 (l, bs) fun)) (fun (Node x Empty Empty) (Leaf r)), bs)
-- 
-- traverseParent2 (Node x l r, bs) fun =
--     (Node x (fst (traverseParent2 (l, bs) fun)) (fst (traverseParent2 (r, bs) fun)), bs)
-- 
-- traverseParent2 (Node x Empty Empty, bs) fun = (Node x Empty Empty, bs)



parents :: Zipper a b -> [b]
parents (_, []) = []
parents (_, [LeftCrumb x t]) = [x]
parents (_, [RightCrumb x t]) = [x]
parents (t, (LeftCrumb x t2):cs)  = [x] ++ parents (t, cs) 
parents (t, (RightCrumb x t2):cs) = [x] ++ parents (t, cs) 

parentsMaybe :: Maybe (Zipper a b) -> [b]
parentsMaybe Nothing = []
parentsMaybe (Just z) = parents z 


-- parent -> child -> child
exf :: Tree [[Char]] [Char] -> Tree [[Char]] [Char] -> Tree [[Char]] [Char]
exf (Leaf x) c = c
exf (Node x _ _) (Leaf y) = Leaf [x ++ "_LUL"]
exf _ child = child



-- traverseThis (Node x l r, bs) fun =
--     (Node x lt rt, bs) 
--     where
--         leftZipper  = (traverseThis (l,bs) fun)
--         rightZipper = (traverseThis (r,bs) fun)
--         lt = fst leftZipper
--         rt = fst rightZipper


-- treeDepth :: Tree a b -> Int
-- treeDepth Empty  = 0
-- treeDepth Leaf _ = 1
-- treeDepth (Node _ leftSubtree rightSubtree) = 
--   1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

-- traverseDF :: Tree a -> [a]
-- traverseDF Empty        = []
-- traverseDF (Node a l r) = a : (traverseDF l) ++ (traverseDF r)
-- 
-- traverseBF :: Tree a -> [a]
-- traverseBF tree = tbf [tree]
--     where
--         tbf [] = []
--         tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))
--         nodeValue (Node a _ _) = a
--         leftAndRightNodes (Node _ Empty Empty) = []
--         leftAndRightNodes (Node _ Empty b)     = [b]
--         leftAndRightNodes (Node _ a Empty)     = [a]
--         leftAndRightNodes (Node _ a b)         = [a,b]
-- 
-- listFlatToTreeDF :: [a] -> Tree a
-- listFlatToTreeDF [] = Empty
-- listFlatToTreeDF [x] = Node x Empty Empty
-- listFlatToTreeDF l = Node x (listFlatToTreeDF ltx) (listFlatToTreeDF gtx)
--     where
--         m = length l `div` 2
--         x = l !! m
--         ltx = take m l
--         gtx = drop (m+1) l


--listToTreeBF :: [a] -> Tree a

-- demo data
t = Node "parent" (Node "l1" (Leaf ["L_ll1"]) (Leaf ["L_lr1"])) (Node "r1" (Leaf ["L_rl1"]) (Leaf ["L_rr1"]))
zipa = (t, [])
mfoc = return zipa >>= goLeft >>= goRight

-- define:
    -- zipper
    -- goUp, goLeft, goRight, isLeaf
    -- method to locate visualizer based on its name

