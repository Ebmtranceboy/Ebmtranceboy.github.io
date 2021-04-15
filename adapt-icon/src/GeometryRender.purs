module GeometryRender where

import Prelude
import Halogen.Svg.Elements (path, line, text) as S
import Halogen.Svg.Attributes (Color(RGB), CommandArcChoice(..), CommandPositionReference(..), CommandSweepChoice(..), PathCommand, a, attr, d, l, m, printColor, stroke, strokeWidth, x, x1, x2, y, y1, y2, z)
import Halogen.HTML.Core (HTML, AttrName(AttrName))
import Halogen.HTML as HH

import Data.Geometry.Plane (Arc(..), Circle(..), HalfLine(..) , Line
                    , Point(..), RightAngle(..), Segment(..), Vector(..)
                    , aPointOnLine, aVectorOfLine, abs, halfline
                    , length, middle, normalTo, ord, rotated, scale
                    , segment, vector, (<+|), point)
import Data.Enum(toEnum)
import Data.Array (concat)
import Data.String(singleton)
import Data.Maybe(Maybe(..),maybe,fromJust)
import Partial.Unsafe(unsafePartial)
import Data.Sparse.Polynomial((^))
import Math (atan2, pi)

type FontStyle = String
type Position = Number
type Size = Number
type Path = String

line :: forall p i. Position -> Position 
               -> Position -> Position 
               -> Maybe Color -> Size -> HTML p i
line ax ay bx by color size =
 S.line
    [ x1 ax
    , x2 bx
    , y1 ay
    , y2 by
    , stroke color
    , strokeWidth size
    ]
  
text :: forall p i. Position -> Position 
               -> Maybe Color -> FontStyle 
               -> String -> HTML p i
text ax ay color fontStyle str = 
  S.text [ x ax 
         , y ay
         ,  attr (AttrName "style") $ "fill:" <> printColor color <> "; font:" <> fontStyle <> "; "
         ] [HH.text str]     
                   
path :: forall p i. Maybe Color -> Size
                  -> Maybe Color 
                  -> Array PathCommand -> HTML p i
path strokeColor strokeWidth fillColor content = 
  S.path 
    [ d content
    , attr (AttrName "style") $ "stroke:" <> printColor strokeColor
                            <> "; stroke-width:" <> show strokeWidth
                            <> "; fill:" <> printColor fillColor <> ";"
    ]

type Context = 
   { strokeColor :: Maybe Color
   , fillColor :: Maybe Color
   , strokeWidth :: Size
   , fontStyle :: FontStyle
   , textFill :: Maybe Color
   }

defaultContext :: Context
defaultContext = 
  { strokeColor: Just $ RGB 0 0 0
  , fillColor: Nothing -- Just $ RGB 0 0 0
  , strokeWidth: 0.1
  , fontStyle: "italic 15px sans-serif"
  , textFill: Just $ RGB 0 0 0
  }

class Render geo where
  render' :: forall p i. Context -> geo -> Array (HTML p i)

instance renderPoint :: Render (Point Number) where
  render' {strokeColor, strokeWidth, textFill, fontStyle} 
           p@(Point {name, coordinates}) = 
    [ line (abs p - 5.0) (ord p - 5.0)
         (abs p + 5.0) (ord p + 5.0) 
         strokeColor strokeWidth
    , line (abs p - 5.0) (ord p + 5.0)
         (abs p + 5.0) (ord p - 5.0)
         strokeColor strokeWidth
    , text (abs p + 10.0) (ord p - 10.0) 
         textFill fontStyle 
         name
    ]

instance renderHalfLine :: Render (HalfLine Number) where
  render' {strokeColor, strokeWidth} 
           (HalfLine {origin, direction}) = 
    let far = origin <+| scale 10.0 direction
    in [ line (abs origin) (ord origin) 
            (abs far) (ord far) 
            strokeColor strokeWidth
       ]

instance renderLine :: Render (Line Number) where
  render' ctx l = 
    let m = aPointOnLine l
        v = aVectorOfLine l
     in    (render' ctx $ halfline m v)
        <> (render' ctx $ halfline m (scale (-1.0) v))

arrowBluntness = 0.2 :: Number
arrowLength = 20.0 :: Number

arrowTip :: Segment Number-> {at1 :: Point Number, at2 :: Point Number}
arrowTip s@(Segment {origin, extremity, asOriented}) = 
  let v = vector origin extremity
      ang = atan2 (ord v) (abs v)
      v0 = Vector $ (length v)^0
      f theta = 
        let v1 = rotated theta $ Vector $ arrowLength^0
          in origin <+| (rotated ang $ v1 <+| v0)
   in { at1: f (pi - arrowBluntness)
      , at2: f (pi + arrowBluntness)}

withCoord :: (Number -> Number -> PathCommand) -> Point Number-> PathCommand
withCoord c p = c (abs p) (ord p) 

instance renderSegment :: Render (Segment Number) where
  render' {strokeColor, strokeWidth, fontStyle, textFill} 
           s@(Segment {origin,extremity,asOriented}) = 
    let mid = middle "" s
    in [ line (abs origin) (ord origin)
         (abs extremity) (ord extremity)
         strokeColor strokeWidth
       ]
        <> ( maybe [] (\str -> 
                let {at1, at2} = arrowTip s
                in [ path strokeColor strokeWidth strokeColor $
                       [ withCoord (m Abs) at1
                       , withCoord (l Abs) extremity
                       , withCoord (l Abs) at2
                       , z
                       ]
                   ] ) asOriented
           )
        <> ( maybe [] (\str -> 
            [ text (abs mid + 10.0) (ord mid - 10.0) 
                 textFill fontStyle
                 str
            , text (abs mid + 10.0) (ord mid - 23.0) 
                 textFill fontStyle 
                       (if str=="" 
                          then "" 
                          else singleton $ unsafePartial 
                                         $ fromJust 
                                         $ toEnum 0x2192)
           ] ) asOriented
          )

instance renderCircle :: Render (Circle Number) where
  render' {strokeColor, strokeWidth, fillColor} 
           (Circle{center: c,radius}) = 
    [ path strokeColor strokeWidth fillColor $
      [ withCoord (m Abs) $ point "" (abs c - radius) (ord c)
      , withCoord (a Rel radius radius 0.0 Arc1 Sweep0) $ point "" (2.0 * radius) 0.0
      ]
    , path strokeColor strokeWidth fillColor $
      [ withCoord (m Abs) $ point "" (abs c - radius) (ord c)
      , withCoord (a Rel radius radius 0.0 Arc1 Sweep1) $ point "" (2.0 * radius) 0.0
      ]
    ]

instance renderArc :: Render (Arc Number) where
  render' {strokeColor, strokeWidth, fillColor, fontStyle, textFill} 
         (Arc { origin, center, extremity, radius
              , flag, flipped, swapped, asOriented}) = 
    let u = scale (radius / length origin) origin
        pO = center <+| u 
        v = scale (radius / length extremity) extremity
        pE = center <+| v
        a2 = if flipped then Sweep0 else Sweep1
        b = if flipped && swapped then not flag else flag
        a1 = if b then Arc1 else Arc0
        a3 = if b then 1.0 else -1.0
        n = pE <+| (scale (-a3) $ normalTo extremity)
        {at1, at2} = arrowTip $ segment n pE Nothing
        uv = u <+| v
        i = center <+| scale (radius * 0.8 / length uv) uv
         
     in  [ path strokeColor strokeWidth fillColor $
          [ withCoord (m Abs) pO
          , withCoord (a Rel radius radius 0.0 a1 a2) $ point "" (abs pE - abs pO) (ord pE - ord pO)
          ]
         ] 
           <>     
          (maybe [] (\str ->
                 [ path strokeColor strokeWidth strokeColor $
                  [ withCoord (m Abs) at2
                  , withCoord (l Abs) pE
                  , withCoord (l Abs) at1
                  , z
                  ]
                 ] ) asOriented)
           <>
           (maybe [] (\str -> 
                  [text (abs i) (ord i) 
                     textFill fontStyle
                     str]) asOriented)
                     
instance renderRightAngle :: Render (RightAngle Number) where
  render' {strokeColor, strokeWidth, fillColor} 
         (RightAngle {origin, center, extremity, radius}) = 
    let v = scale (radius/length extremity) extremity
        w = scale (radius/length origin) origin
        u = v <+| w
        a = center <+| u
        b = center <+| v
        c = center <+| w
       in [ path strokeColor strokeWidth fillColor $
            [ withCoord (m Abs) c
            , withCoord (l Abs) a
            , withCoord (l Abs) b
            ]
          ]

instance renderSequence :: Render a => Render (Array a) where        
  render' ctx arr = concat $ (render' ctx) <$> arr

