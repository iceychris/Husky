module Visualizers where

-- 3rd party
import qualified Data.Vector.Storable as V
import Graphics.Vty

-- local
import Model
import Util



visEmpty :: Visualizer -> Husky -> Image
visEmpty _ _ = string defAttr "visEmpty"

visDummy :: Visualizer -> Husky -> Image
visDummy v _ = string defAttr "visDummy" 

visPower :: Visualizer -> Husky -> Image
visPower v h = 
    cr " " <-> cr "Power: " <-> cr barB
    where
        cr = string defAttr
        aud = audio h
        last = head $ audioFFTSqHistory aud
        maxi = V.maximum last
        barA = show maxi
        barB = barApplied $ displayable maxi 20.0

visVInfo :: Visualizer -> Husky -> Image
visVInfo v h = string defAttr $ vis_name v

visHInfo :: Visualizer -> Husky -> Image
visHInfo v h = string defAttr $ description h

visRawValues :: Visualizer -> Husky -> Image
visRawValues v h = string defAttr $ show $ V.take 3 $ audioMemFFT $ audio h

-- draw a simple box in the 
-- bounds specified by the visualizer
visBox :: Visualizer -> Husky -> Image
visBox v h = img 
    where
        w = 20 
        h = 20
        rowUpDown = string defAttr $ replicate w 'X'
        rowMid = string defAttr $ "X" ++ replicate (w - 2) ' ' ++ "X"
        allMid = vertCat $ replicate (h - 2) rowMid
        img = vertJoin rowUpDown $ vertJoin allMid rowUpDown


------------
-- Visualizers
------------

mkVis name f = Visualizer {
    vis_name = name,
    visualize = f
}

vInfo = mkVis "info" visDummy
vHInfo = mkVis "husky info" visHInfo
vVInfo = mkVis "visualizer info" visVInfo
vPower = mkVis "power" visPower
vBox = mkVis "Box" visBox
vRaw = mkVis "Raw" visRawValues

