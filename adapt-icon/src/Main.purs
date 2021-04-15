module Main where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Data.Int (toNumber, round)
import Data.Foldable (foldl, fold)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Halogen.Svg.Elements (svg, path, circle) as S
import Halogen.Svg.Attributes (Color(RGB), CommandPositionReference(..), d, fill, h, height, m, stroke, strokeWidth, v, viewBox, width, z, cx, cy, r)
import SimpleAbsSVG (ShortPath (..))
import SimpleAbsSVG as SA

import Data.Array as Array
import Bivariate (Bi(..), atan, bic, bix, biy)
import Math (pi)
import Data.Geometry.Plane (Arc(..), Segment(..), length, point, projection, vector, segment, arc, (<+|), inner, det)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    void $ runUI page unit body

page ∷ forall m. Monad m => H.Component (Const Void) Unit Void m 
page = 
  H.mkComponent 
    { initialState: const {}
    , eval: H.mkEval $ H.defaultEval
      { handleAction = const $ pure mempty 
      }
    , render
    }

shape :: Array (ShortPath Number)
shape = 
  [ M 11.7 17.5
  , V 11.223
  , H 8.5
  , V 9.986
  , H 11.76
  , V 7.33
  , L 11.016 6.596
  , V 6.0
  , H 13.182
  , L 13.997 6.412
  , V 9.986
  , L 22.428 9.963
  , L 23.268 10.788
  , L 18.096 15.874
  , A 3.91 true 23.325 19.311
  , A 6.02 true 22.871 23.503
  , A 4.037 true 20.961 25.381
  , A 6.291 true 15.019 24.931
  , A 3.291 true 14.193 21.975
  , A 1.343 true 15.951 24.002
  , A 2.93 false 20.935 22.869
  , A 4.9 false 20.901 18.527
  , A 2.93 false 19.794 17.152
  , A 2.99 false 16.813 17.164
  , L 15.788 17.668
  , V 17.164
  , L 20.4 11.116
  , H 14.018
  , V 17.393
  , A 2.08 false 16.51 18.813
  , A 3.76 false 17.395 17.92
  , A 0.342 true 17.512 17.77
  , A 0.236 true 17.966 17.839
  , A 0.619 true 18.129 18.263
  , A 2.92 true 16.976 19.913
  , A 2.748 true 15.439 20.371
  , A 3.92 true 11.7 17.5
  , Z
  ]

geometry :: Array (SA.Geometric Bi)
geometry = Array.catMaybes $ SA.toGeometric <$> (SA.longPath $ (bic <$> _) <$> shape)

type Bool = Bi

heaviside :: Bi -> Bool
heaviside b = (atan (bic 100.0 * b) * bic (2.0 / pi) + bic 1.0) / bic 2.0

less :: Bi -> Bi -> Bool
less a b = heaviside (b - a)

minimum :: Bi -> Bi -> Bi
minimum a b = a * less a b + b * less b a

and :: Bool -> Bool -> Bool
and p q = p * q

compl :: Bool -> Bool
compl p = bic 1.0 - p

or :: Bool -> Bool -> Bool
or p q = compl (and (compl p) (compl q))

ifte :: Bool -> Bi -> Bi -> Bi
ifte c t e = t * c + e * (compl c)

mag :: Bi -> Bi
mag x = x * less (bic 0.0) x - x * less x (bic 0.0)

distanceSegment :: Segment Bi -> Bi -> Bi -> Bi
distanceSegment s@(Segment { origin, extremity, asOriented }) bx by
  = do
    let p = point "" bx by
        v = vector origin p 
        h = origin <+| projection (vector origin extremity) v
        c1 = less (bic 0.0) (inner (vector origin h) (vector origin extremity))
        c2 = less (bic 0.0) (inner (vector extremity h) (vector extremity origin))
    ifte 
      (and c1 c2) 
      (length (vector p h))
      (minimum (length $ vector p origin) (length $ vector p extremity))
       
distanceArc :: Arc Bi -> Bi -> Bi -> Bi
distanceArc a@(Arc { origin, center, extremity, radius, flag, flipped , swapped, asOriented }) bx by
  = do
    let p = point "" bx by
        d = det origin extremity
        l = - det extremity (vector center p) / d
        m = det origin (vector center p) / d
        c1 = less (bic 0.0) l
        c2 = less (bic 0.0) m
    ifte 
      (and c1 c2)
      (mag $ (length $ vector p center) - radius)
      (minimum (length $ vector p $ center <+| origin) (length $ vector p $ center <+| extremity))
      
distanceGeometric :: SA.Geometric Bi -> Bi -> Bi -> Bi
distanceGeometric (SA.GeoS s) = distanceSegment s
distanceGeometric (SA.GeoA a) = distanceArc a

showGeo (SA.GeoS (Segment { origin, extremity, asOriented })) = "Segment (" <> show origin <> ", " <> show extremity <> ")"
showGeo (SA.GeoA (Arc { origin, center, extremity, flipped, swapped, flag, asOriented })) = "Arc (" <> show (center <+| origin) <> ", " <> show center <>", " <> show (center <+| extremity) <> ")"

distanceFunction :: Array (SA.Geometric Bi) -> Bi -> Bi -> Bi
distanceFunction arr bx by 
  = case Array.uncons arr of
        Just { head, tail } -> 
          foldl (\ m g -> minimum m $ distanceGeometric g bx by) 
            (distanceGeometric head bx by)
            tail
        _ -> bic 0.0

ori = point "" (bic 1.0) (bic 0.0)
ext = point "" (bic 0.0) (bic 1.0)
center = point "" (bic 0.0) (bic 0.0)
vec = vector ori center
pro = ori <+| projection (vector ori ext) vec
s = segment ori ext Nothing
a = arc (vector center ori) center (vector center ext) (bic 1.0) false false false Nothing

render :: forall m a r. r -> H.ComponentHTML a () m
render _ = do
  HH.div_
    [ figure
    , HH.label_ 
      [ HH.text $ show $ distanceSegment s (bic 0.0) (bic 0.0)
      , HH.br_
      , HH.text $ show $ distanceArc a (bic 0.5) (bic 0.5)
      , HH.br_
      , HH.text $ show $ distanceFunction geometry (bic 8.0) (bic 5.0)
      ]
    ]
  
figure :: forall m a. HH.HTML a m
figure = do
  S.svg
          [ viewBox 0.0 0.0 30.0 30.0
          , width 400.0
          , height 400.0
          ] $
          (SA.simpleAbsRender $ SA.longPath shape)
          <>
          [ S.path 
              [ d 
                [ m Abs 8.0 5.0
                , v Abs 27.0
                , h Abs 24.0
                , v Abs 5.0
                ,z
                ]
              , stroke $ Just $ RGB 0 0 0
              , strokeWidth 0.1
              , fill Nothing
              ]
              
            , S.path 
              [ d $ SA.toPathCommand <$> shape
              , stroke Nothing 
              , fill $ Just $ RGB 240 240 240
              ]
          ]
          {-
          <> (Array.concat $ (\ i -> (\ j -> 
                let x = 8.0 + toNumber i * 16.0 / 30.0 
                    y = 5.0 + toNumber j * 22.0 / 30.0
                    Bi { d0, d1, d2 } = distanceFunction geometry (bix x) (biy y)
                    f = round $ d0 * 100.0
                in S.circle [ cx x
                       , cy y 
                       , r 0.2
                       , stroke Nothing
                       , fill $ Just $ RGB f f f
                       ]
                               ) <$> (0 Array... 30) 
                       ) <$> (0 Array... 30)
             )
             -} 
 
