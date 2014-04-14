import Graphics.Gloss.Interface.Pure.Display
import Graphics.Gloss.Data.Vector

import Data.List
import System.Environment
import System.Random

data Poly = Poly Integer [Integer] deriving (Eq, Show)

some_poly :: Integer -> Int -> IO Poly
some_poly mod deg = do
    coeffs <- sequence 
        ( replicate (succ deg) $ randomRIO (0,pred mod) )
    return $ Poly mod coeffs

infinite_trail :: Poly -> [Poly]
infinite_trail p = p : infinite_trail
   ( let Poly m cs = p
     in  Poly m $ map (\ c -> mod c m )
                $ zipWith (+) cs ( tail cs ++ [0] ) )

angle :: Poly -> Float
angle (Poly mod cs) = 
    2 * pi * fromInteger (head cs) / fromInteger mod

angles :: Poly -> [Float]
angles p = map angle 
         $ p : takeWhile (/= p) (tail $ infinite_trail p)

some_interesting_poly mod deg = do
    p <- some_poly mod deg
    if sub_period p > 10
    then return p else some_interesting_poly mod deg

sub_period p = 
    let full = succ $ length $ takeWhile (/= p) 
             $ tail $ infinite_trail p
        sub  = succ $ length $ takeWhile ( not . eq_lin p )
             $ tail $ infinite_trail p
    in  div full sub

eq_lin (Poly _ cs) (Poly _ ds) = 
    and $ zipWith (==) (tail cs) (tail ds)

{-

-- that's the idea but it does not work because of OpenGL
-- implementation restrictions (transformation stack depth < 32)

picture pol = Color white $
    foldl ( \ pic ang -> Translate (-1) 0
              $ Pictures [ Rotate ang pic , Line [ (0,0), (1,0) ]] )
          Blank $ angles pol
-}

walk as = 
    let (acc, ys) = 
             mapAccumL ( \ acc x -> ( plusV acc x, acc ) ) (0,0) 
           $ map unitVectorAtAngle as
    in  ys ++ [acc]

plusV (a,b)(c,d) = (a+c,b+d)
minusV (a,b)(c,d) = (a-c,b-d)

bbox ps = ( (minimum $ map fst ps, minimum $ map snd ps)
          , (maximum $ map fst ps, maximum $ map snd ps)
          )

main :: IO ()
main = do
    m <- randomRIO (10 :: Integer,100)

    pol <- some_interesting_poly 510 3
    print $ sub_period pol

    let points = walk $ angles pol
        bb @ ((u,l),(d,r)) = bbox points
        cnt = mid (u,l) (d,r)
        ext = max (d-u) (r-l) ; s = fromIntegral width / ext
        width = 600
    print bb
    display (InWindow "Q" (width,width) (0,0)) black 
        $ Color white 
        $ Scale s s
        $ Translate (negate $ fst cnt) (negate $ snd cnt)
        $ Line points
               

mid (a,b) (c,d) = ((a+c)/2, (b+d)/2 )
