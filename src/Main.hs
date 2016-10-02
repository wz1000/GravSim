{-# LANGUAGE DeriveGeneric #-}
module Main where

import Graphics.Gloss
import System.Random
import Data.Monoid
import Control.Monad
import GHC.Generics
import Control.Parallel.Strategies
import Debug.Trace

type World = [Particle]

data Particle = P { mass :: !Float, position :: !Position, velocity :: !Velocity, prevP :: [Position] } deriving (Show, Generic)

instance NFData Particle

instance Monoid Particle where
        mempty = P 0 (0,0) (0, 0) []
        mappend (P m1 (x1, y1) (xv1,yv1) xs) (P m2 (x2, y2) (xv2, yv2) ys) = P (m1+m2) (x, y) (xv, yv) (if m1 > m2 then xs else ys)
                                                                        where x = normalise x1 x2
                                                                              y = normalise y1 y2
                                                                              xv = normalise xv1 xv2
                                                                              yv = normalise yv1 yv2
                                                                              normalise a b = (m1*a + m2*b)/(m1+m2)

type Position = (Float, Float)
type Acceleration = (Float, Float)
type Velocity = (Float, Float)

window = InWindow "Test" (1000, 1000) (0, 0)

randomBall :: Position -> Position -> IO Particle
randomBall a b = do
        m <- randomRIO (1, 1000000)
        x <- randomRIO a
        y <- randomRIO b
        [xv, yv] <- replicateM 2 $ randomRIO (-00, 00)
        return $ P m (x, y) (xv, yv) []

main = do
        x <- replicateM 50 $ randomBall (-15000, -5000) (-5000, 5000)
        y <- replicateM 50 $ randomBall (15000, 5000) (-5000, 5000)
        z <- replicateM 50 $ randomBall (-5000, 5000) (-15000, 5000)
        z' <- replicateM 55 $ randomBall (-5000, 5000) (15000, 5000)
        let a = P 9000000 (-1000, 0) ( 0, 0) [] `giveOrbitalVel` head b : map (`giveOrbitalVel` head a) x 
            b = P 500000  (1000, 0 ) ( 0,-0) [] `giveOrbitalVel` head d : map (`giveOrbitalVel` head b) y 
            c = P 900000  (0,-1000 ) (-0, 0) [] `giveOrbitalVel` head d : map (`giveOrbitalVel` head c) z 
            d = P 50000   (0, 1000 ) ( 0, 0) [] `giveOrbitalVel` head c : map (`giveOrbitalVel` head d) z' 
            world = a ++ b ++ c ++ d
        simulate window white 60 world render (const step)

gravConstant :: Float
gravConstant = 500

radius :: Particle -> Float
radius = (**(1/3)). mass

distance :: Particle -> Particle -> Float
distance (P _ (x1, y1) _ _) (P _ (x2, y2) _ _) = sqrt ((x2-x1)**2 + (y2-y1)**2)

giveOrbitalVel :: Particle -> Particle -> Particle
giveOrbitalVel p1 p2 = p1 { velocity = (vx, vy) }
                        where v = sqrt (gravConstant * mass p2 / dist)
                              dist = distance p1 p2
                              vx = negate $ v * y / dist
                              vy = v * x / dist
                              x = x2 - x1
                              y = y2 - y1
                              (x1, y1) = position p1
                              (x2, y2) = position p2

force :: Particle -> Particle -> Acceleration
(P m1 (x1, y1) _ _) `force` (P m2 (x2, y2) _ _) = if dist < 10 then (0,0) else (fX, fY)
        where dx = x2 - x1
              dy = y2 - y1
              dist = sqrt $ (dx * dx) + (dy * dy)
              f = gravConstant * m2 / (dist * dist)
              fX = f * dx/dist
              fY = f * dy/dist


step :: Float -> World -> World
step dt world = world''' `using` parList rdeepseq
                where world' = map (\(P m (x, y) (xv, yv) xs) -> P m (x+xv*dt, y+yv*dt) (xv, yv) ((x,y):xs)) world `using` parList rdeepseq
                      world'' = do
                              b@(P m p (xv, yv) xs) <- world'
                              let (xa, ya) = foldr (\(a, b) (c, d) -> (a+c, b+d)) (0, 0) $ map (force b) world' 
                              return $ P m p (xv + xa*dt, yv + ya*dt) xs
                      world''' = map mconcat $ foldr insert [] world''
                      insert b [] = [[b]]
                      insert b (x : xs) = if any (collidesWith b) x
                                             then (b:x):xs
                                             else x : insert b xs

collidesWith :: Particle -> Particle -> Bool
collidesWith a b = distance a b <= maximum [radius a, radius b]

renderParticle :: Particle -> Picture
renderParticle (P mass (x, y) _ xs) = line (xs) <> (Translate x y $ circleSolid (mass ** (1/3)))

render :: World -> Picture
render = foldMap renderParticle
