module SimpleAbsSVG where

import Prelude

import Data.Array as Array
import Data.Geometry.Plane (class Real, Segment, Arc, segment, point, arc, circle, meets, vector, middle)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\),(/\))
import GeometryRender (defaultContext, render')
import Halogen.HTML (HTML) 
import Halogen.Svg.Attributes (CommandArcChoice(..), CommandPositionReference(..), CommandSweepChoice(..), PathCommand, a, h, l, m, v, z)

data ShortPath k = M k k | H k | V k | L k k | A k Boolean k k | Z

instance functorShortPath :: Functor ShortPath where
  map f (M x y) = M (f x) (f y)
  map f (H x) = H (f x)
  map f (V y) = V (f y)
  map f (L x y) = L (f x) (f y)
  map f (A r b x y) = A (f r) b (f x) (f y)
  map f Z = Z

type LongPath k = k /\ k /\ ShortPath k

longPath :: forall k. Array (ShortPath k) -> Array (LongPath k) 
longPath shape
  = do
      let set (_ /\ oriy) (H x) = x /\ oriy
          set (orix /\ _) (V y) = orix /\ y
          set (_ /\ _) (L x y) = x /\ y
          set (_ /\ _) (A r b x y) = x /\ y 
          set (_ /\ _) (M x y) = x /\ y
          set (x /\ y) Z = x /\ y
          oris 
            = case Array.uncons shape of 
                   Just { head: M x y, tail } -> Array.scanl set (x /\ y) (M x y Array.: shape)
                   _ -> []
          zs = Array.zip oris shape
      (\ ((x /\ y) /\ p) -> x /\ y /\ p) <$> zs 

data Geometric k = GeoS (Segment k) | GeoA (Arc k)

toPathCommand :: ShortPath Number -> PathCommand
toPathCommand (M x y) = m Abs x y
toPathCommand (H x) = h Abs x
toPathCommand (V y) = v Abs y
toPathCommand (L x y) = l Abs x y
toPathCommand (A r b x y) = a Abs r r 0.0 Arc0 (if b then Sweep1 else Sweep0) x y
toPathCommand Z = z

toGeometric :: forall k. Eq k => Ord k => Ring k => EuclideanRing k => Real k => LongPath k -> Maybe (Geometric k)
toGeometric (ox /\ oy /\ L x y) = Just $ GeoS $ segment (point "" ox oy) (point "" x y) Nothing
toGeometric (ox /\ oy /\ H x) = Just $ GeoS $ segment (point "" ox oy) (point "" x oy) Nothing
toGeometric (ox /\ oy /\ V y) = Just $ GeoS $ segment (point "" ox oy) (point "" ox y) Nothing
toGeometric (ox /\ oy /\ A r b x y) 
  = do
    let c1 = circle (point "" ox oy) r
        c2 = circle (point "" x y) r
        ms = c1 `meets` c2
        center 
          = case Array.length ms of
                 1 -> Array.head ms
                 2 -> if b 
                         then ms Array.!! 0
                         else ms Array.!! 1
                 _ -> middle <$> Just "" <*> (segment <$> (ms Array.!! 0) <*> (ms Array.!! 1) <*> Nothing)
    center >>= (\ c -> Just $ GeoA $ arc (vector c $ point "" ox oy) c (vector c $ point "" x y) r false (not b) false Nothing)
toGeometric _ = Nothing

simpleAbsRender :: forall p i. Array (LongPath Number) -> Array (HTML p i)
simpleAbsRender lp = do
  let arr = Array.catMaybes $ toGeometric <$> lp
      f (GeoS s) = render' defaultContext s
      f (GeoA a) = render' defaultContext a
  Array.concat $ f <$> arr

