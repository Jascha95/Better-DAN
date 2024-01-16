{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Pics where
import Data.Monoid ((<>))
import Graphics.Svg as S

data Coord = Coord { x :: Float, y :: Float }
  deriving (Eq,Show)

data Picture =
    Line {a::Coord, b::Coord}
  | Circle {center::Coord, radius::Float}
  | Combine [Picture]
  deriving (Eq, Show)

instance Monoid Picture where
  mempty = Combine []
  mappend a b = Combine [a, b]

c x y = Coord {x, y}
line a b = Line {a, b}

circ center radius = Circle {center, radius}

polyline (x:y:t) = line x y <> polyline (y:t)
polyline _ = mempty

rect (Coord {x, y}) len height =
  polyline [ c x y, c (x+len) y, c (x+len) (y+height), c x (y+height), c x y ]

  -- Operation on coordinates

negC (Coord {x,y}) = Coord {x = -x, y = -y}
(+/) c1 c2 = Coord { x = x c1 + x c2, y = y c1 + y c2 }
(*/) c q = Coord { x = x c * q, y = y c * q }
(°/) (Coord {x,y}) theta = Coord {x = x * cos theta - y * sin theta, y = x * sin theta + y * cos theta}


-- Operation on Pictures

mapPic f g pic = case pic of
  Line {a, b} -> line (f a) (f b)
  Circle {center, radius} -> circ (f center) (g radius)
  Combine l -> Combine (map (mapPic f g) l)

moveP v = mapPic (+/v) id
scaleP q = mapPic (*/q) (*q)
rotate0 theta = mapPic (°/theta) id
rotateP theta center pic = moveP center $ rotate0 theta $ moveP (negC center) pic

-- Example of pictures

triangle = polyline [c 0 100, c 100 100, c 50 0, c 0 100]

basic :: Picture
basic = rect (c 0 0) 100 100 <> triangle

house :: Picture
house =
  rect (c 0 0) (100) (-100)
  <> line (c 0 0) (c 100 (-100))
  <> line (c 100 0) (c 0 (-100))
  <> moveP (c 0 (-200)) triangle
  <> moveP (c 0 (-200)) (circ (c 50 70) 25)

mkLandscape l =
  foldr f mempty l
  where
    f q pic = scaleP q house <> moveP (c (q*100) 0) pic

landscape = mkLandscape [0.5, 0.7, 1, 0.9, 0.3, 0.8]

dragon' 1 len = (line (c 0 0) (c len 0), (c len 0))
dragon' n len =
  (new_gen, new_endpoint)
  where (prev_gen, endpoint) = dragon' (n-1) len
        new_endpoint = endpoint °/ (pi/4) */ (sqrt 2)
        new_gen = prev_gen <> rotateP (-pi/2) endpoint prev_gen

dragon n l = fst $ dragon' n l

-- Svg export

toSvg :: Picture -> S.Element
toSvg pic = case pic of
  Line {a, b} -> path_ [D_ <<- mA (x a) (y a) <> lA (x b) (y b)]
  Circle {center, radius} ->
    circle_ [Cx_ <<- (toText $ x center),
             Cy_ <<- (toText $ y center),
             R_ <<- toText radius]
  Combine l -> mconcat $ map toSvg l

output path pic =
  let svg = doctype <>
            with (svg11_ (toSvg $ moveP (c 0 200) pic)) [Stroke_ <<- "black", Fill_ <<- "none",
           Width_ <<- "1200", Height_ <<- "980"] in
  renderToFile path svg

{- How to use:

> output "/tmp/house.svg" $ mkLandscape [0.5, 0.9, 0.8, 0.6, 0.3]
> output "/tmp/dragon.svg" $ move (c 500 500) $ rotate0 (pi/2) $ dragon 14 8

-}
