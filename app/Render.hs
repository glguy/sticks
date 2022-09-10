module Render (renderBlock) where

import Block

renderBlock :: Block Bool -> String
renderBlock (Block x1 x2 x3 x4 x5 x6) =
    renderStick x1 "translate <-1,0,0>\n" ++
    renderStick x2 "translate < 1,0,0>\n" ++
    renderStick x3 "rotate <90, 180, 0>\ntranslate < 0,1,0>\n" ++
    renderStick x4 "rotate <90, 180, 0>\ntranslate < 0,-1,0>\n" ++
    renderStick x5 "rotate <90, 90, 0>\ntranslate <0,0,-1>\n" ++
    renderStick x6 "rotate <90, 90, 0>\ntranslate <0,0,1>\n"

renderStick :: Stick Bool -> String -> String
renderStick (Stick f l b r) tx =
        "difference {\n" ++
        "  cylinder {\n" ++
        "    <0, -3, 0>,\n" ++
        "    <0,  3, 0>,\n" ++
        "    0.4\n" ++
        "    texture { T_Wood25 scale 1 }\n" ++
        "  }\n" ++
        renderSide f (-0.5) 0.5 (-0.5) (-0.5) ++
        renderSide l (-0.5) (-0.5) (-0.5) 0.5 ++
        renderSide b (-0.5) 0.5 0.5 0.5 ++
        renderSide r 0.5 0.5 (-0.5) 0.5 ++
        tx ++
        "}\n"

renderSide :: Side Bool -> Double -> Double -> Double -> Double -> String
renderSide (Side x y z w v) x1 x2 z1 z2 =
    renderCut x x1 x2 2 z1 z2 ++
    renderCut y x1 x2 1 z1 z2 ++
    renderCut z x1 x2 0 z1 z2 ++
    renderCut w x1 x2 (-1) z1 z2 ++
    renderCut v x1 x2 (-2) z1 z2

renderCut :: Bool -> Double -> Double -> Double -> Double -> Double -> String
renderCut False _ _ _ _ _ = ""
renderCut True x1 x2 y z1 z2 =
    "  cylinder {\n" ++
    "    <" ++ show x1 ++ ", " ++ show y ++ ", " ++ show z1 ++ ">,\n" ++
    "    <" ++ show x2 ++ ", " ++ show y ++ ", " ++ show z2 ++ ">,\n" ++
    "    0.4\n" ++
    "    texture { T_Wood24 scale 1 }\n" ++
    "  }\n"
