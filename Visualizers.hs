module Visualizers where

-- 3rd party
import Graphics.Vty

-- local
import Model


visEmpty :: Visualizer -> Audio -> Image
visEmpty v a = string defAttr ""

-- i dont know about that one...
info = Visualizer {
    vis_name = "Info Visualizer",
    vis_width = 40,
    vis_height = 10
}

visDummy :: Visualizer -> Husky -> Audio -> Image
visDummy v _ _ = string defAttr ("| dummy | " ++ vis_name v ++ " | (" ++ (show $ vis_width v) ++ "," ++ (show $ vis_height v) ++ ")")

visInfo :: Husky -> Visualizer -> Image
visInfo h v = string defAttr $ vis_name v

visMetrics :: Husky -> Visualizer -> Image
visMetrics h v = string defAttr $ description h

-- draw a simple box in the 
-- bounds specified by the visualizer
visBox :: Husky -> Visualizer -> Image
visBox h v = img 
    where
        w = vis_width v
        h = vis_height v
        rowUpDown = string defAttr $ replicate w 'X'
        rowMid = string defAttr $ "X" ++ replicate (w - 2) ' ' ++ "X"
        allMid = vertCat $ replicate (h - 2) rowMid
        img = vertJoin rowUpDown $ vertJoin allMid rowUpDown


