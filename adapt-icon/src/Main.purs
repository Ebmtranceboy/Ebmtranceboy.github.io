module Main where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Svg.Elements (svg, path) as S
import Halogen.Svg.Attributes (Color(RGB), CommandPositionReference(..), d, fill, h, height, m, stroke, strokeWidth, v, viewBox, width, z)
import SimpleAbsSVG (ShortPath (..))
import SimpleAbsSVG as SA

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

shape :: Array SA.ShortPath
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

render :: forall m a r. r -> H.ComponentHTML a () m
render _ = do
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
          
