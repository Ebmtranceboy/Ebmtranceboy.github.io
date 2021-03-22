module Main where

import Prelude
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.Svg.Elements (svg, path)
import Halogen.Svg.Attributes (Color(RGB), CommandArcChoice(..), CommandPositionReference(..), CommandSweepChoice(..), a, d, fill, h, height, l, m, stroke, strokeWidth, v, viewBox, width, z)

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

render :: forall m a r. r -> H.ComponentHTML a () m
render _ = do
  svg
          [ viewBox 0.0 0.0 30.0 30.0
          , width 400.0
          , height 400.0
          ]
          [ path 
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
            , path 
              [ d
                [ m Abs 11.7 17.5
                , v Rel (-6.277)
                , h Rel (-3.2)
                , v Rel (-1.237)
                , h Rel 3.26 
                , v Rel (-2.656)
                , l Rel (-0.744) (-0.734)
                , v Rel (-0.596)
                , h Rel 2.166
                , l Rel 0.815 0.412
                , v Rel 3.574
                , l Rel 8.431 (-0.023)
                , l Rel 0.84 0.825
                , l Rel (-5.172) 5.086
                , a Rel 3.51 3.51 0.0 Arc0 Sweep1 5.229 3.437
                , a Rel 7.16 7.16 0.0 Arc0 Sweep1 0.175 1.466 
                , a Rel 6.02 6.02 0.0 Arc0 Sweep1 (-0.629) 2.726 
                , a Rel 4.037 4.037 0.0 Arc0 Sweep1 (-1.91) 1.878
                , a Rel 6.291 6.291 0.0 Arc0 Sweep1 (-5.942) (-0.45)
                , a Rel 3.291 3.291 0.0 Arc0 Sweep1 (-0.826) (-2.956)
                , a Rel 1.27 1.27 0.0 Arc0 Sweep1 (1.758) 2.027 
                , a Rel 2.93 2.93 0.0 Arc0 Sweep0 4.984 (-1.133)
                , a Rel 4.9 4.9 0.0 Arc0 Sweep0 (-0.034) (-4.342)
                , a Rel 2.93 2.93 0.0 Arc0 Sweep0 (-1.107) (-1.375)
                , a Rel 2.99 2.99 0.0 Arc0 Sweep0 (-2.981) 0.012
                , l Rel (-1.025) 0.504
                , v Rel (-0.504)
                , l Rel 4.612 (-6.048)
                , h Rel (-6.382)
                , v Rel 6.277
                , a Rel 2.08 2.08 0.0 Arc0 Sweep0  2.492 1.42
                , a Rel 3.76 3.76 0.0 Arc0 Sweep0 0.885 (-0.893)
                , a Rel 0.342 0.342 0.0 Arc0 Sweep1 0.117 (-0.15) 
                , a Rel 0.236 0.236 0.0 Arc0 Sweep1 0.454 0.069
                , a Rel 0.619 0.619 0.0 Arc0 Sweep1 0.163 0.424 
                , a Rel 2.92 2.92 0.0 Arc0 Sweep1  (-1.153) 1.65
                , a Rel 2.748 2.748 0.0 Arc0 Sweep1 (-1.537) 0.458
                , a Rel 3.92 3.92 0.0 Arc0 Sweep1 (-3.715) (-2.802)
                , z 
                ]
              , stroke $ Just $ RGB 0 0 0
              , strokeWidth 0.05
              , fill $ Just $ RGB 240 240 240
              ]
          ]
