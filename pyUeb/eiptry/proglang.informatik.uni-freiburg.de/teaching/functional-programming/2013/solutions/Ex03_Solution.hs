module Ex03_Solution where
import Prelude hiding (log)
import Data.List
  
import LogTypes
import Canvas

-- |--------------------|
-- | Highscores
-- |--------------------|

  
instance Ord HighScoreEntry where
  compare (HSE _ won) (HSE _ won') = compare won won'

data Game = G { start :: Time, end :: Time, players :: (Player, Player),
                res1 :: Maybe Choice, res2 :: Maybe Choice }
  deriving (Show, Eq)

                 
reconstructHS :: Log -> HighScore
reconstructHS log@(Log glog _) = reverse $ sort winStats
  where -- map players to win-count
        winStats = [ HSE p (winsOf p) | p <- allPlayers ]
        -- calculate win-count
        winsOf p = length $ filter (==p) wins 
        -- list of wins players
        wins = [p | Just p <- gameResults ]
        wins' = map fromJust $ filter isJust gameResults
        wins'' = catMaybes gameResults


        -- evaluate each game
        gameResults = map evalGame $ reconstructGames log

        -- collect all the players
        allPlayers = nub $ map player1 glog ++ map player2 glog
                         
        -- Determine if a game was one, and by whom
        evalGame (G _ _ (p1, p2) (Just r1) (Just r2))
          | r1 `beats` r2 = Just p1
          | r2 `beats` r1 = Just p2
          | otherwise = Nothing
        evalGame _ = Nothing


        beats Schere Papier = True
        beats Papier Stein = True
        beats Stein Schere = True
        beats _ _ = False
                             
-- re-implementation from Data.Maybe
fromJust (Just x) = x
fromJust _ = error "Not Just!"

isJust (Just _) = True
isJust Nothing = False

catMaybes [] = []
catMaybes (Just x:xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs



-- collect the game events that belong to one game (we assume that a
-- player always participates at only one game at a time)
matchupGames glog = [ preGame g (find (doesStop g) glog) | g <- glog ]
  where doesStop (GLE st p1 p2 Start) (GLE et p1' p2' Stop) =
          p1 == p1' && p2 == p2' && et >= st
        doesStop _ _ = False

        preGame (GLE st p1 p2 _) (Just (GLE et _ _ _)) =
          Just $ (st, et, p1, p2)
        preGame _ Nothing = Nothing

                     
-- reconstruct the games from the logs
reconstructGames (Log glog pls) =
  [ G st et (p1, p2)(result $ sliceLog st et pl1)
                    (result $ sliceLog st et pl2)
  | Just (st, et, p1, p2) <- matchupGames glog
  -- alternative
  -- , (st', et', p1', p2') <- catMaybes $ matchupGames glog
  , (p1'', pl1) <-  pls, p1'' == p1
  , (p2'', pl2) <-  pls, p2'' == p2
  ]
  where sliceLog :: Time -> Time -> PlayerLog -> PlayerLog
        sliceLog st et pl = [ ple | ple@(t, _) <- pl, t >= st && t <= et ]

        doesStop (GLE st p1 p2 Start) (GLE et p1' p2' Stop) =
          p1 == p1' && p2 == p2' && et >= st
        doesStop _ _ = False

        result :: PlayerLog -> Maybe Choice
        result [] = Nothing
        result xs = Just $ snd $ last xs



-- |--------------------|
-- | Vector
-- |--------------------|

data V2 a = V { vX :: a, vY :: a}
  deriving (Show, Eq)


-- Some Operations on vectors
toTuple :: V2 a -> (a, a)
toTuple (V x y) = (x, y)

vPlus :: Num a => V2 a -> V2 a -> V2 a
vPlus (V x1 y1) (V x2 y2) = V (x1 + x2) (y1 + y2)

vFromScalar :: Num a => a -> V2 a
vFromScalar i = V i 0

vMult :: Num a => V2 a -> V2 a -> V2 a
vMult (V a b) (V c d) = V (a * c - b * d) (a * d + b * c)

vNeg :: Num a => V2 a -> V2 a
vNeg (V x y) = V (-x) (-y)

vAbs :: Floating a => V2 a -> V2 a
vAbs (V x y) = vFromScalar (sqrt (x*x + y*y))

vSig :: Num a => V2 a -> V2 a
vSig (V x y) = vFromScalar (signum x)

instance Floating a => Num (V2 a) where
  (+) = vPlus
  (*) = vMult
  negate = vNeg
  abs = vAbs
  signum = vSig
  fromInteger = vFromScalar . fromInteger
                            
type V2D = V2 Double


-- |--------------------|
-- | Pictures
-- |--------------------|

-- The picture datatype
data Picture = Line V2D -- ^ from, to defined by vector
             | Rect Double Double -- ^ width, height
             | Circle Double -- ^ radius
             | Ontop Picture Picture -- ^ put first picture above the second
             | Translate V2D Picture -- ^ Move a picture by a vector
  deriving (Show, Eq)

(<+>) = Ontop

ex_pic1 :: Picture
ex_pic1 = Line (V 7 7) <+> (Translate (V 5 5) (Circle 5)) <+> Rect 10 10

cross :: Double -> Double -> Picture
cross w h = Line (V w h) <+> (Translate (V 0 h) (Line (V w (-h))))

triangle :: Double -> Double -> Picture
triangle w h = Line (V w 0)
               <+> Line (V half h)
               <+> (Translate (V half h) (Line (V half (-h))))
  where half = w/2
                                                                 
centeredTriangle w h = Translate (V (-halfW) (-halfH)) (triangle w h)
  where halfW = w/2
        halfH = h/2

-- Das Haus vom Nikolaus
ex_pic2 :: Picture
ex_pic2 = Rect 10 10
          <+> 
          cross 10 10
          <+> 
          (Translate (V 0 10) $ triangle 10 5)
        


scale :: Double -> Picture -> Picture
scale f (Line v) = Line (vFromScalar f * v)
scale f (Rect w h) = Rect (f*w) (f*h)
scale f (Circle r) = Circle (f*r)
scale f (Ontop p1 p2) = Ontop (scale f p1) (scale f p2)
scale f (Translate v p) = Translate (vFromScalar f * v) (scale f p)

ex_pic3 :: Picture
ex_pic3 = scale 10 $ ex_pic1 <+> Translate (V 20 0) ex_pic2


draw :: V2D -> Picture -> Script
draw from (Line to) = drawLine (toTuple from) (toTuple (from `vPlus` to))
draw from (Rect w h) = drawRect (toTuple from) w h
draw from (Circle r) = drawCircle (toTuple from) r
draw from (Ontop p1 p2) = draw from p1 ++ draw from p2
draw from (Translate v p) = draw (from `vPlus` v) p


