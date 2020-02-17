module SVGpork.Render where

import Prelude

import Data.Enum (toEnum)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Sparse.Polynomial ((^))
import Data.String (singleton)
import Math (atan2, pi)
import Partial.Unsafe (unsafePartial)
import SVGpork.Geometry (Arc(..), Circle(..), HalfLine(..), Line, Point(..), RightAngle(..), Segment(..), Vector(..), aPointOnLine, aVectorOfLine, abs, halfline, length, middle, ord, projection, rotated, scale, segment, vector, (<+|))
import Spork.Html (Html)
import Spork.Html as H

type Color = String
type FontStyle = String
type Position = Number
type Size = Number
type Path = String

ns :: Maybe H.Namespace
ns = Just $ H.Namespace "http://www.w3.org/2000/svg"

svgline :: forall action. Position -> Position
        -> Position -> Position
         -> Color -> Size -> Html action
svgline x1 y1 x2 y2 stroke strokeWidth =
  H.elemWithNS ns "line"
      [ H.attr "x1" $ show x1
      , H.attr "y1" $ show y1
      , H.attr "x2" $ show x2
      , H.attr "y2" $ show y2
      , H.attr "style" $ "stroke:" <> stroke
                    <> "; stroke-width:" <> show strokeWidth <> "px;"
      ] []

svgtext :: forall action. Position -> Position
                       -> Color -> FontStyle
                       -> String -> Html action
svgtext x y fill fontStyle text =
  H.elemWithNS ns "text"
    [ H.attr "x" $ show x
    , H.attr "y" $ show y
    , H.attr "style" $ "fill:" <> fill
                    <> "; font:" <> fontStyle <> ";"
    ] [H.text text]


svgpath :: forall action. Color -> Size
                       -> Color -> Path -> Html action
svgpath stroke strokeWidth fill path =
  H.elemWithNS ns "path"
    [ H.attr "d" path
    , H.attr "style" $ "stroke:" <> stroke
                    <> "; stroke-width:" <> show strokeWidth <> "px;"
                    <> "; fill:" <> fill <> ";"
    ] []

type Context =
   { stroke :: Color
   , fill :: Color
   , strokeWidth :: Size
   , fontStyle :: FontStyle
   , textFill :: Color}

defaultContext :: Context
defaultContext =
  { stroke: "#000"
  , fill: "#00000000"
  , strokeWidth: 1.5
  , fontStyle: "italic 15px arial, sans-serif"
  , textFill: "#000"
  }

class Render a where
  render' :: forall action. Context -> a -> Array (Html action)

instance renderPoint :: Render Point where
  render' {stroke, strokeWidth, textFill, fontStyle}
           p@(Point {name, coordinates}) =
    [ svgline (abs p - 5.0) (ord p - 5.0)
                (abs p + 5.0) (ord p + 5.0)
                stroke strokeWidth
    , svgline (abs p - 5.0) (ord p + 5.0)
                (abs p + 5.0) (ord p - 5.0)
                stroke strokeWidth
    , svgtext (abs p + 10.0) (ord p - 10.0)
                textFill fontStyle
                name
    ]

instance renderHalfLine :: Render HalfLine where
  render' {stroke, strokeWidth}
           (HalfLine {origin, direction}) =
    let far = origin <+| scale 10.0 direction
    in [svgline (abs origin) (ord origin)
                   (abs far) (ord far)
                   stroke strokeWidth]

instance renderLine :: Render Line where
  render' ctx l =
    let m = aPointOnLine l
        v = aVectorOfLine l
     in render' ctx [ halfline m v
                    , halfline m (scale (-1.0) v)]

arrowBluntness = 0.3 :: Number
arrowLength = 20.0 :: Number

arrowTip :: Segment -> {at1 :: Point, at2 :: Point}
arrowTip s@(Segment {origin, extremity, asOriented}) =
  let v = vector origin extremity
      ang = atan2 (ord v) (abs v)
      v0 = Vector $ (length v)^0
      f theta =
        let v1 = rotated theta $ Vector $ arrowLength^0
          in origin <+| (rotated ang $ v1 <+| v0)
   in { at1: f (pi - arrowBluntness)
      , at2: f (pi + arrowBluntness)}

pathCoord :: Point -> String
pathCoord p = " " <> (show $ abs p) <> " " <> (show $ ord p) <> " "

instance renderSegment :: Render Segment where
  render' {stroke, strokeWidth, fontStyle, textFill}
           s@(Segment {origin,extremity,asOriented}) =
    let m = middle "" s
    in [ svgline (abs origin) (ord origin)
                (abs extremity) (ord extremity)
                stroke strokeWidth]
       <>
        maybe [] (\str ->
          let {at1, at2} = arrowTip s
          in [svgpath stroke strokeWidth stroke $
                 "M" <> pathCoord at1
              <> "L" <> pathCoord extremity
              <> "L" <> pathCoord at2 <> "Z" ] ) asOriented
       <>
         maybe [] (\str ->
            [ svgtext (abs m + 10.0) (ord m - 10.0)
                        textFill fontStyle
                        str
            , svgtext (abs m + 10.0) (ord m - 23.0)
                        textFill fontStyle
                                 (if str==""
                                    then ""
                                    else singleton $ unsafePartial
                                                   $ fromJust
                                                   $ toEnum 0x2192) ] )
                                                              asOriented

instance renderCircle :: Render Circle where
  render' {stroke, strokeWidth, fill}
           (Circle{center: c,radius}) =
    [ svgpath stroke strokeWidth fill $
            "M " <> (show $ abs c - radius) <> " " <> (show $ ord c) <> " "
         <> "a " <> (show radius) <> " " <> (show radius) <> " "
                 <> "0 1 0 " <> (show $ 2.0 * radius) <> " 0"
    , svgpath stroke strokeWidth fill $
            "M " <> (show $ abs c - radius) <> " " <> (show $ ord c) <> " "
         <> "a " <> (show radius) <> " " <> (show radius) <> " "
                 <> "0 1 1 " <> (show $ 2.0 * radius) <> " 0"
    ]

instance renderArc :: Render Arc where
  render' {stroke, strokeWidth, fill, fontStyle, textFill}
         (Arc { origin, center, extremity, radius
              , flag, flipped, swapped, asOriented}) =
    let u = scale (radius / length origin) origin
        pO = center <+| u
        v = scale (radius / length extremity) extremity
        pE = center <+| v
        a2 = if flipped then "0 " else "1 "
        b = if flipped && swapped then not flag else flag
        a1 = if b then "1 " else "0 "
        a3 = if b then 1.0 else -1.0
        d = 0.8 - 0.004 * radius
        p = (scale (a3*d) v)
                      <+| origin
                      <+| (scale (-1.0) $ projection extremity origin)
        n = pE <+| (scale (-a3) p)
        {at1, at2} = arrowTip $ segment n pE Nothing
        uv = u <+| v
        i = center <+| scale (radius * 0.8 / length uv) uv

     in [svgpath stroke strokeWidth fill $
                "M" <> pathCoord pO
                    <> "a " <> show radius <> " " <> show radius <> " "
                    <> "0 " <> a1 <> a2 <> " "
                    <> (show $ abs pE - abs pO) <> " "
                    <> (show $ ord pE - ord pO)]
              <>
               maybe [] (\str ->
                 [svgpath stroke strokeWidth stroke $
                        "M" <> pathCoord at2
                            <> "L" <> pathCoord pE
                            <> "L" <> pathCoord at1 <> "Z" ] ) asOriented
              <>
                maybe [] (\str ->
                  [svgtext (abs i) (ord i)
                     textFill fontStyle
                     str] ) asOriented

instance renderRightAngle :: Render RightAngle where
  render' {stroke, strokeWidth, fill}
         (RightAngle {origin, center, extremity, radius}) =
    let v = scale (radius/length extremity) extremity
        w = scale (radius/length origin) origin
        u = v <+| w
        m = center <+| u
        n = center <+| v
        o = center <+| w
     in [svgpath stroke strokeWidth fill $
             "M" <> pathCoord o
          <> "L" <> pathCoord m
          <> "L" <> pathCoord n  ]

instance renderSequence :: Render a => Render (Array a) where
  render' ctx arr = foldr (<>) mempty $ (render' ctx) <$> arr
