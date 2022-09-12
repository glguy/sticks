module Render (animate) where

import Control.Lens ( index, set )
import Text.Printf ( printf )

import Block
import PathSolver (Action(..), applyAction)

renderAnimation :: (Int, Action) -> Block Bool -> String
renderAnimation (i,a) (Block x1 x2 x3 x4 x5 x6) =
    renderStick x1 (f 0 ++ "translate <-1,0,0>\n") ++
    renderStick x2 (f 1 ++ "translate < 1,0,0>\n") ++
    renderStick x3 (f 2 ++ "rotate <90, 0, 0>\ntranslate < 0,1,0>\n") ++
    renderStick x4 (f 3 ++ "rotate <90, 0, 0>\ntranslate < 0,-1,0>\n") ++
    renderStick x5 (f 4 ++ "rotate <90, 90, 0>\ntranslate <0,0,-1>\n") ++
    renderStick x6 (f 5 ++ "rotate <90, 90, 0>\ntranslate <0,0,1>\n")
    where
        f j
            | i == j =
                case a of
                    ActLeft     -> "rotate <0, 90*Cl, 0>\n"
                    ActRight    -> "rotate <0, -90*Cl, 0>\n"
                    ActUp       -> "translate <0, Cl, 0>\n"
                    ActDown     -> "translate <0, -Cl, 0>\n"
                    ActInsert{} -> "translate <0, 8*(1-Cl), 0>\n"
            | otherwise = ""

renderBlock :: Block Bool -> String
renderBlock (Block x1 x2 x3 x4 x5 x6) =
    renderStick x1 "translate <-1,0,0>\n" ++
    renderStick x2 "translate < 1,0,0>\n" ++
    renderStick x3 "rotate <90, 0, 0>\ntranslate < 0,1,0>\n" ++
    renderStick x4 "rotate <90, 0, 0>\ntranslate < 0,-1,0>\n" ++
    renderStick x5 "rotate <90, 90, 0>\ntranslate <0,0,-1>\n" ++
    renderStick x6 "rotate <90, 90, 0>\ntranslate <0,0,1>\n"

renderStick :: Stick Bool -> String -> String
renderStick s _ | s == noStick = ""
renderStick (Stick lo mi hi f l b r) tx =
    "difference {\n\
    \  cylinder {\n\
    \    <0, -4.5, 0>,\n\
    \    <0,  4.5, 0>,\n\
    \    0.5\n\
    \    texture { T_Wood25 scale 1 }\n\
    \  }\n" ++
    renderSide f (-cutR) cutR (-cutR) (-cutR) ++
    renderSide l (-cutR) (-cutR) (-cutR) cutR ++
    renderSide b (-cutR) cutR cutR cutR ++
    renderSide r cutR cutR (-cutR) cutR ++
    shft ++
    tx ++
    "}\n"
    where
        cutR = 0.6
        shft
            | lo = "translate <0,-1,0>\n"
            | mi = ""
            | hi = "translate <0,1,0>\n"
            | otherwise = error "renderStick: invalid stick"

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
    printf
    "  cylinder {\n\
    \    <%f,%f,%f>,\n\
    \    <%f,%f,%f>,\n\
    \    0.5\n\
    \    texture { T_Wood24 scale 1 }\n\
    \  }\n"
    x1 y z1 x2 y z2

animate :: [(Int, Action)] -> String
animate path = go 0 path (pure True)
    where
        go :: Int -> [(Int, Action)] -> Block Bool -> String
        go _ [] b = renderBlock b
        go t ((i,a):as) b =
            printf
            "#if (clock < %d)\n\
            \#declare Cl = clock - %d;\n\
            \%s\
            \#else\n\
            \%s\
            \#end\n"
            (t+1) t (renderAnimation (i,a) b') (go (t+1) as (applyAction (i,a) b'))
            where
                b' = case a of
                    ActInsert s -> set (sticks . index i) s b
                    _ -> b
