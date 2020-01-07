import Linear
import System.Environment
import Data.List
import Data.Maybe
type Color = V3 Double

rescale :: Double -> Int
rescale = floor . (255.99*)

fromVec :: V3 Double -> V3 Int
fromVec = fmap rescale

makePixel :: Color -> String
makePixel v = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"
  where (V3 r g b) = fromVec v

makeGradient :: Color -> Color -> Double -> Color
makeGradient c1 c2 t = lerp t c2 c1

myGrad :: Color -> Color -> Int -> [Color]
myGrad c1 c2 n = map (makeGradient c1 c2 . (/fromIntegral n) . fromIntegral) [0..n]

-- Floyd-Steinberg Dithering
-- fsd :: [Color] -- 1D picture
    -- -> [Color] -- Color palate
    -- -> [Color]
-- fsd as cs n =

-- The above is overkill. Let's simplify.
fsd :: Int
    -> [Double]
    -> [Double]
fsd n []  = []
fsd n [p]  = [(/n') . fromIntegral . round $ (n'*p)]
  where n' = fromIntegral n
fsd n (p:q:ps) = p':fsd n (q + (p-p'):ps) 
  where p' = (/n') . fromIntegral . round $ (n'* p)
        n' = fromIntegral n

fsd' :: Int
    -> [Double]
    -> [Double]
fsd' n []  = []
fsd' n [p]  = return .(/n') . fromIntegral . round $ (n'*p)
  where n' = fromIntegral n
fsd' n [p,q] = p':(fsd' n . return) (q + (p-p')) 
  where p' = (/n') . fromIntegral . round $ (n'* p)
        n' = fromIntegral n
fsd' n (p:q:r:ps) = p':fsd' n (q + 0.75*(p-p'):r+0.25*(p-p'):ps) 
  where p' = (/n') . fromIntegral . round $ (n'* p)
        n' = fromIntegral n

fsdcOld :: [Color] -- Colors to work with
     -> [Color] -- Original gradient.
     -> [Color] -- FSD'ed gradient.
fsdcOld cs ps =
  let closestNbr p = head . sortBy
        (\x -> \y -> quadrance (x-p) `compare` (quadrance (y-p))) $ cs
  in case ps of 
    []     -> []
    [p]    -> return . closestNbr $ p
    p:q:qs -> p' : (fsdcOld cs) (q + 0.6*^(p - p'):qs)
      where p' = closestNbr p

fsdc :: [Color] -- Colors to work with; first and last are special.
      -> [Color] -- Original gradient.
      -> [Color] -- FSD'ed gradient.
fsdc cs ps =
  let c1 = head cs
      c2 = last cs
      proj2line v = c1 + project (c2-c1) (v-c1)
      closestNbr p = head . sortBy
        (\x -> \y -> quadrance (proj2line x-p) `compare` (quadrance (proj2line y-p))) $ cs
  in case ps of 
    []     -> []
    [p]    -> return . closestNbr $ p
    p:q:qs -> p' : (fsdc cs) (q + 0.8*^(p - p'):qs)
      where p' = closestNbr p

fsdc' :: [Color] -- Colors to work with; first and last are special.
      -> [Color] -- Original gradient.
      -> [Int] -- FSD'ed gradient.
fsdc' cs ps =
  let c1 = head cs
      c2 = last cs
      proj2line v = c1 + project (c2-c1) (v-c1)
      closestNbr p = head . sortBy
        (\x -> \y -> quadrance (proj2line x-p) `compare` (quadrance (proj2line y-p))) $ cs
  in case ps of 
    []     -> []
    [p]    -> return . fromJust . flip elemIndex cs. closestNbr $ p
    p:q:qs -> (fromJust . flip elemIndex cs ) p' : (fsdc' cs) (q + 0.8*^(p - p'):qs)
      where p' =  closestNbr $ p


discretize :: [Color] -- Colors to work with
           -> [Color] -- Original gradient.
           -> [Color] -- discretized gradient.
discretize cs ps =
  let proj2line v = c1 + project (c2-c1) (v-c1)
      closestNbr p = head . sortBy
        (\x -> \y -> quadrance (proj2line x-p) `compare` (quadrance (proj2line y-p))) $ cs
  in map closestNbr ps

discretize' :: [Color] -- Colors to work with
            -> [Color] -- Original gradient.
            -> [Color] -- discretized gradient.
discretize' cs ps =
  let closestNbr p = head . sortBy
        (\x -> \y -> quadrance (x-p) `compare` (quadrance (y-p))) $ cs
  in map closestNbr ps

-- c1 = V3 0 0.1 0.3; c2 = V3 0.1 0.5 0.7
-- c1 = V3 0 0 0; c2 = V3 1 1 1
-- [c1,c2] = map (1/255*^) [V3 27 40 83, V3 47 140 132]
-- [c2,c1] = map (1/255*^) [V3 27 40 83, V3 47 140 132]
-- [c1,c2] = map (1/255*^) [V3 27 40 83, V3 19 71 125]
{- palate = map (1/255*^)
  [V3 27 40 83
  ,V3 37 59 115
  ,V3 19 71 125
  ,V3 52 127 140
  ,V3 52 125 117
  ,V3 47 140 132
  ] -}
[c1,c2] = map (2*^)
-- [c2,c1] = map (2*^)
  [V3 0.062 0.047 0.066
  -- ,V3 0.126 0.351 0.322
  ,V3 0.119 0.186 0.271
  -- ,V3 0.074 0.159 0.155
  ]

palate =  map (2*^)
  [V3 0.062 0.047 0.066
  ,V3 0.075 0.068 0.107
  ,V3 0.129 0.117 0.224
  -- ,V3 0.074 0.159 0.155
  ,V3 0.119 0.186 0.271
  -- ,V3 0.126 0.351 0.322
  ]
image n = map (/fromIntegral n)  [1..fromIntegral n]
-- image n = map ((/2) . (1-) . cos . (*(0.5*2*pi/fromIntegral n)))  [1..fromIntegral n]
-- image n = map (
  -- \θ->let
      -- θ'= 2*pi*θ/fromIntegral n
     -- in (1-(1-(1-cos θ')/2)^2)/2 + (1-cos (θ'/2))/4
            -- ) [1..fromIntegral n]
makePicture n h = header ++ body
  where
    header = "P3\n"
      ++ show n ++ " " ++ show (length bodies*h) ++ "# width height (pixels)\n"
      ++ "255 # max intensity\n\n"
    process k f = concat . concat . take k . repeat
      . return . concat. map makePixel .  f . map (makeGradient c1 c2) 
    body = concat bodies
    bodies = map (($image n) . process h)
      [fsdc palate
      --,fsdcOld palate
      ,id
      ,discretize palate
      ]

makeKey :: Int -> [(Int,Int)]
makeKey n = foldr (
             \c -> \ps ->
               case ps of
                 []              -> [(c,1)]
                 pp@((c',n):pps) ->
                   if c == c'
                     then (c',n+1):pps
                     else (c,1):pp
        ) [] .fsdc' palate . map (makeGradient c1 c2) . image $ n 
      -- ,id ]
    -- body1 = concat. concat . take h . repeat $ [concat . map (makePixel . makeGradient c1 c2) . fsd' 1 . take n $ repeat 0.5]
    -- body2 = concat. concat . take h . repeat $ [concat . map (makePixel . makeGradient c1 c2) . take n $ repeat 0.5]

main = do
  n <- getProgName
  let key = makeKey 420
  writeFile (n++".ppm") $ makePicture (round $ 420) 100
  writeFile (n++".txt") . show $ key
  putStrLn . show $
    map (\n->sum . map snd . filter ((==n) . fst) $ key) [0..5]
