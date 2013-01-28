module RayTrace (rayTrace, (<+>), (/>),
	Color, Point3D, 
	ObjectsTree (..), Resolution, Scene (..), Texture (..), Object (..), Diff (..), Light (..), Ray (..), Camera (..),
	textureBluePlane
	) where

import Maybe
import Foreign

-- Variable for antialising, the bigger value, the more time it will take. Multiples of 4
antialising :: Int
antialising = 4

type Point2D = (Int,Int)
type Point3D = (Double,Double,Double)
type Vector  = (Double,Double,Double)

data Ray = Ray Point3D !Vector

data Object = Sphere Double !Point3D
            | Plane (Double,Double,Double,Double)  

type Resolution = (Int,Int)
type Dimension  = (Int,Int)

(<+>), (<->), (<*>) :: (Double,Double,Double) -> (Double,Double,Double) -> (Double,Double,Double)
(x1,y1,z1) <+> (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
(x1,y1,z1) <-> (x2,y2,z2) = (x1-x2,y1-y2,z1-z2)
(x1,y1,z1) <*> (x2,y2,z2) = (x1*x2,y1*y2,z1*z2)

(*>), (/>) :: (Double,Double,Double) -> Double -> (Double,Double,Double)
(x,y,z) *> f = (x*f,y*f,z*f)
(x,y,z) /> f = (x/f,y/f,z/f)

maxF, minF :: Double -> (Double,Double,Double) -> (Double,Double,Double)
maxF f (x,y,z) = (max x f, max y f, max z f)
minF f (x,y,z) = (min x f, min y f, min z f)

(*.) :: Vector -> Vector -> Double
(x1,y1,z1) *. (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

len :: Vector -> Double
len v = sqrt (v *. v)

norm :: Vector -> Vector
norm v
   | len v < 10**(-9) = (0.0,0.0,0.0)
   | otherwise = v *> (1/(len v))

mkNormVect :: Point3D -> Point3D -> Vector
mkNormVect v w = norm (w <-> v)

dist :: Point3D -> Point3D -> Double
dist p0 p1 = sqrt ((p1 <-> p0) *. (p1 <-> p0))

clip :: (Double,Double,Double) -> (Double,Double,Double)
clip = (maxF 0.0) . (minF 1.0)

solveq :: (Double,Double,Double) -> [Double]
solveq (a,b,c)
   | (d < 0)   = []
   | (d > 0)   = [(- b - sqrt d)/(2*a), (- b + sqrt d)/(2*a)]
   | otherwise = [-b/(2*a)]
 where
   d = b*b - 4*a*c

mkRay :: Point3D -> Point3D -> Ray
mkRay p1 p2 = Ray p1 (mkNormVect p1 p2)

intRayWith (Ray start dir) (Sphere rad cen) = (fstPos cross, edge)
   where
       d = start <-> cen
       cross = solveq (dir *. dir, 2*(dir *. d), (d *. d) - rad^2)
       edge = reallyEdge cross -- Edge, two intersection points are very close to each other, or almost equal
         where 
               reallyEdge ds | length ds == 2 = rad - sqrt(rad^2 - ((ds!!1 - ds!!0)/2)^2) < coef
                             | otherwise = length ds == 1
               coef = sqrt (rad) * 0.3
intRayWith (Ray start dir) (Plane (a,b,c,d)) = (fstPos (if (abs(part) < 10**(-9)) then [] else [- (d + ((a,b,c) *. start) ) / part]), False)
  where
      part = (a,b,c) *. dir

normal :: Point3D -> Object -> Vector
normal p (Sphere rad cen) = norm ((p <-> cen) *> (1/rad))
normal _ (Plane (a,b,c,d)) = norm (a,b,c)

reflectDir :: Vector -> Vector -> Vector
reflectDir i n = i <-> (n *> (2*(n *. i)))

refractDir :: Vector -> Vector -> Double -> Vector
refractDir i n r = if (v < 0) then (0.0, 0.0, 0.0) else norm $ (i *> r_c) <+> (n *> (r_c*(abs c) - sqrt v))
  where
      c   = n *. (i *> (-1))
      r_c = if (c < 0) then r else 1/r -- when cosVal < 0, inside of sphere (so travelling to vacuum)
      v   = 1 + (r_c^2) * (c^2 - 1)

mapToWin :: Resolution -> Dimension -> Point2D -> Point3D
mapToWin (rx,ry) (w,h) (px,py) = (x/rxD,y/ryD,0.0)
  where
      (rxD,ryD) = (fromIntegral rx, fromIntegral ry)
      (pxD,pyD) = (fromIntegral px, fromIntegral py)
      (wD,hD)   = (fromIntegral w, fromIntegral h)
      (x,y)     = ( (pxD-rxD/2)*wD, (pyD-ryD/2)*hD )

type Color = (Double,Double,Double)

data Diff = Solid Color |
            Bitmap (Point3D -> Color)

data Texture = Texture Diff Double Int Double Double

type TexturedObject = (Object,Texture)

data ObjectsTree = Empty |
                     Node (Object, [ObjectsTree]) |
                     Data TexturedObject

type Intensity = (Double,Double,Double)

data Light = PointLight Point3D Intensity
           | AmbientLight Intensity

data Camera = Camera Point3D Dimension

data Scene = Scene Camera Color [ObjectsTree] [Light]

data Intersection = IntersectionTextured Double Bool Ray TexturedObject |
                    IntersectionObject Double Ray Object [ObjectsTree]

type Image = Point2D -> Color

intDist :: (Maybe Intersection) -> Double
intDist Nothing = 0.0
intDist (Just (IntersectionTextured d _ _ _)) = d
intDist (Just (IntersectionObject d _ _ _)) = d

isEdge :: (Maybe Intersection) -> Bool
isEdge (Just (IntersectionTextured _ edge _ _)) = edge
isEdge _ = False

intText :: (Maybe Intersection) -> Texture
intText Nothing = Texture (Solid (0.0,0.0,0.0)) 0.0 0 0.0 0.0
intText (Just (IntersectionTextured _ _ _ (_,t))) = t
intText (Just (IntersectionObject _ _ _ _ )) = Texture (Solid (0.0,0.0,0.0)) 0.0 0 0.0 0.0

colorAt :: (Maybe Intersection) -> Color
colorAt Nothing   = (0.0,0.0,0.0)
colorAt (Just (IntersectionTextured _ _ _ (_,Texture (Solid color) _ _ _ _) )) = color
colorAt i@(Just (IntersectionTextured _ _ _ (_,Texture (Bitmap f) _ _ _ _) )) = f (intPt i)

normalAt :: (Maybe Intersection) -> Vector
normalAt Nothing   = (0.0,0.0,0.0)
normalAt i@(Just (IntersectionTextured _ _ _ (o,_) )) = normal (intPt i) o

intPt :: (Maybe Intersection) -> Point3D
intPt Nothing = (0.0,0.0,0.0)
intPt (Just (IntersectionTextured d _ (Ray start dir) _)) = start <+> (dir *> d)

fstPos :: [Double] -> Double
fstPos   []   = 0.0
fstPos (l:ls) = if l > 0 then l else fstPos ls

touch :: Ray -> ObjectsTree -> (Maybe Intersection)
touch r Empty = Nothing
touch r (Node (o,leaf)) = if d > 10**(-6)
                        then Just (IntersectionObject d r o leaf)
                        else Nothing
  where
      (d, edge) = intRayWith r o
touch r (Data (o,m))    = if d > 10**(-6)
                        then Just (IntersectionTextured d edge r (o,m))
                        else Nothing
  where
      (d, edge) = intRayWith r o

-- Quick sort
qsort []     = []
qsort (x:xs) = qsort (filter (\y -> intDist x > intDist y) xs) ++ [x] ++ qsort (filter (\y -> intDist x <=  intDist y) xs)

intersect :: Ray -> [ObjectsTree] -> (Maybe Intersection) 
intersect r ns = work $ qsort $ filter (\i -> not $ isNothing i) (map (touch r) ns)
  where
    work [] = Nothing
    work ((Just (IntersectionObject _ _ _ leaf)):ns) = work $ qsort $ filter (\i -> not $ isNothing i) ((intersect r leaf) : ns)
    work (n:ns) = n


diff :: (Maybe Intersection) -> Light -> Color
diff _ (AmbientLight _)     = (0.0,0.0,0.0)
diff i (PointLight pos int) = (int *> ((mkNormVect (intPt i) pos) *. (normalAt i))) <*> (colorAt i)

avg :: [Double] -> Double
avg list = (foldr (+) 0 list) / (fromIntegral $ length(list))

spec :: (Maybe Intersection) -> Vector -> Light -> Color
spec _ _ (AmbientLight _)     = (0.0,0.0,0.0)
spec i d (PointLight pos int) = int *> (reflCoef * ( ((normalAt i) *. h)**(fromIntegral specCoef) ))
     where
                h = norm ((d *> (-1)) <+> (mkNormVect (intPt i) pos))
                (Texture _ reflCoef specCoef _ _) = intText i  

shadePt :: Intersection -> Vector -> [ObjectsTree] -> Light -> Color 
shadePt i d o (AmbientLight int) = int
shadePt i d o l@(PointLight pos int) -- = (colorAt (Just i))
	-- Shadow
        | s  && not (isEdge (i_s))    = (0.0,0.0,0.0)
        | s                           = avg [shadePt' i d o (around l x y) | x <- [-(antialising' / 4), (antialising' / 4)], y <- [-(antialising' / 4), (antialising' / 4)]]
--      | s                           = (1.0,1.0,1.0) -- Can be used to display detected shadow edge with white color
        | otherwise = (diff (Just i) l)  <+> (spec (Just i) d l)
        where 
                antialising' = fromIntegral antialising
		-- Is Shadow?
                s     = not (isNothing i_s) && (intDist i_s) <= dist (intPt (Just i)) pos 
                -- Light intersection with _objectsTree_ - finding for shadow (if another _object_ can be reached faster with _light_)
                i_s = intersect (mkRay (intPt (Just i)) pos) o 
                -- Helpers
		around (PointLight (x,y,z) int) xp yp = PointLight (x+2*xp, y+2*yp, z) int
		avg ds = (foldr (<+>) (0.0, 0.0, 0.0) ds) /> (fromIntegral $ length ds)
               
		shadePt' i d o (AmbientLight int) = int
		shadePt' i d o l@(PointLight pos int) -- = (colorAt (Just i))
			-- Shadow
			| s         = (0.0,0.0,0.0)
			| otherwise = (diff (Just i) l)  <+> (spec (Just i) d l)
			where 
				-- Is Shadow?
				s     = not (isNothing i_s) && (intDist i_s) <= dist (intPt (Just i)) pos 
				-- Light intersection with _objectsTree_ - finding for shadow (if another _object_ can be reached faster with _light_)
				i_s = intersect (mkRay (intPt (Just i)) pos) o 

reflectPt :: Int -> Intersection -> Vector -> [ObjectsTree] -> [Light] -> Color 
reflectPt depth i d = colorPt depth (Ray (intPt (Just i)) (reflectDir d (normalAt (Just i)))) (0.0,0.0,0.0) 

refractPt :: Int -> Intersection -> Vector -> Color -> [ObjectsTree] -> [Light] -> Color  
refractPt depth i d b = if refractedDir == (0.0,0.0,0.0) then (\x y -> (0.0,0.0,0.0))  
                                                         else colorPt depth (Ray (intPt (Just i)) refractedDir) (b *> refrCoef) 
   where 
        refractedDir = refractDir d (normalAt (Just i)) refrIndex  
        (Texture _ _ _ refrCoef refrIndex) = intText (Just i) 

colorPt :: Int -> Ray -> Color -> [ObjectsTree] -> [Light] -> Color
colorPt (-1) _ _ _ _ = (0.0, 0.0, 0.0) 
colorPt d r@(Ray _ dir) b o l = if (isNothing i) then b else clip $ shadeColor <+> reflectColor <+> refractColor --Clip ensures that all points of color are 0<=x<=1 (or 255 in RGB)
   where 
       shadeColor   = foldl (<+>) (0.0,0.0,0.0) (map (shadePt (fromJust i) dir objects) l)
       reflectColor = if (reflCoef == 0.0) then (0.0, 0.0, 0.0) 
                                           else (reflectPt (d-1) (fromJust i) dir objects l) *> reflCoef
       refractColor = if (refrCoef == 0.0) then (0.0, 0.0, 0.0) 
                                           else (refractPt (d-1) (fromJust i) dir b objects l) *> refrCoef 
       i = intersect r o
       objects = o
       (Texture _ reflCoef _ refrCoef _) = intText i 

rayTracePt :: Int -> Resolution -> Scene -> Point2D -> Color
rayTracePt d r s@(Scene (Camera eye dim) b o l) p = 
      if (isNothing i) 
        then 
          b 
        else 
            if (isEdge i == False) 
             then
               colorPt d (ray p) b o l
             else 
               -- Antialiasing using points around point in top left, top right, bottom left and bottom right dirrections
               avg [colorPt d (ray (around p x y)) b o l | x <- [-(antialising `div` 4), (antialising `div` 4)], y <- [-(antialising `div` 4), (antialising `div` 4)]]
   where 
       ray p = Ray (mapToWin r dim p) (mkNormVect eye (mapToWin r dim p))
       i = intersect (ray p) o
       avg ds = (foldr (<+>) (0.0, 0.0, 0.0) ds) /> (fromIntegral $ length ds)
       around (x,y) xp yp = (x+xp, y+yp)

rayTrace :: Int -> Resolution -> Scene -> Image 
rayTrace d r s = rayTracePt d r s

{- Some textures -}
textureBluePlane :: Point3D -> Color
textureBluePlane (x,y,z) = (0, 0 , s)
 where
   q = sqrt (x^2+y^2)
   s = [0.1, 0.2..1.0]!!(fromIntegral (round(q * 0.5) `mod` 10))
