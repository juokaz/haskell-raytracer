-- Juozas Kaziukenas (s0820151) competition entry

{-

Some explanations.

My goal was to implement program which can generate very beautiful images in very low processing power usage (< 2 min. generation time, as low RAM as possible). This story explains how I reached my goals.

I started by reading this tutorial (http://www.haskell.org/tmrwiki/HRay) and trying to create my own bits with it. My first renders took almost 1:30 hour to render 500x500 image with not more than 10 objects. After realizing that using ghci+emacs is slower than direct compiling with ghc, it took only 10 min.

Soon I discovered that it is very inefficient algorithm, so I started hacking it. 

First fing was to implement spational partitioning or group objects into tree. Spational partitioning is mostly used as bsp-tree, but since I used fractal based structures this was not the best solution. So I started analyzing my test renders and soon discovered that my objects can be "easily" grouped in to trees. I'm not going to explain tree structure deeply, but it may look like list with n-depth sub-lists: [1, [2,7], [3,[5,6], [9,[10,[11]]]]. At first iteration of is-ray-hit-object I check only first level objects, then decide if it intersected any, if yes - try to do it with child elements and so on, I do it until I reach object (not group of them) or empty space. It works very fast, since it only checks objects which are possibly in specific pixel and not all objects in space.

After implemention of tree structure it dropped to 1:30 min. Goal reached, but why don't do more?

Next thing was parallel Haskell. After some chats in #inf.eduni and Googling, I successfuly implemented parallel mapping function which reduced generation time to 50 sec. (about 40%) Since I gained some extra time (2 min. goal - 50 sec. = 1:10 min.) I implemented two minor functions - antialiasing and soft shadows. Both use antialiasing idea to check surrounding points to make transitions between colors more soft. But first implementations checked 8 more points for every single pixel, so it required 9 times more operations. After some thinking, I realized that this can be easily optimized with edges detection. Now I antialiase only such pixel which is near spheres edges - other points don't need antialising since colors inside sphere surface doesn't change. Time increased only to 1:30 min. So paralleling payed off.

I recorded my record time 1:30 min. as the best time for (this image is my contest entry):
1000x1000 resolution
2091 objects (5 depth recursion + plane)
2 lights
2 cores laptop

After finishing my program, I successfuly ran it with even 10'000x10'000 resulution and more than 600k objects (screenshots available (30mb), email me), which took about 2 hours on my laptop. Generation time can be improved by using student.compute, since it has 4 cores compared to my 2. Also, I couldn't run my program with bigger objects count, because I have only 1 Gb of ram (already ordered additional 2 Gb (: ) and system uses Swap space, which reduces generation speed to almost 0. But, my program can be run with no matter how big scene you have as long as you have enough ram.

Key points of my discoveries:
1. My programs run's fast. Very fast if you correctly group objects to tree.
2. Speed depends on resolution, not on objects count. Objects count makes difference, but not very big, compared to resolution change.
3. Paralleling math operations can give you 100% * N (processors count) increased speed.

So, my program started from RayTrace tutorial and grew into usefull engine for 3D fractals. It may seem that many code is still from tutorial, but I think I modified the most important things and now it's simply not worth rewriting left part by myself. I'm very happy about what I have done, because I've learned so much about graphics, 3d vectors practical usage, opengl, raytracing, optimization, parallelizm, bytestrings (not used here), file output strems and so on. I haven't done *any* OpenGL graphics or even RayTracing at all, so this competition was huge chalenge. And most of my motivitaion don't even come from competition, but self interest in doing something more than exams. It was worth it.

My program features:
* Raytracing engine for reflections, refractions and shadows with lightning
* Antialiasing with given level
* "Bitmap" textures for objects - functions
* Soft edges of shadows
* Objects Tree grouping
* Parallel threads
* PPM or GLFW output
* Fractal generation by given depth
* Low memory usage (only for storing initial scene)
* Many places for future improvements

To run it, just execute "compile.sh" (which will compile program with threading support), and then run "run.sh" (edit it, to specify number of processors to use)

-}

-- OpenGL and GLFW for graphics in windows
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
-- System input/output
import System.IO
-- Open Window and etc.
import Pictures
-- Raytrace
import RayTrace
-- Parallel
import Control.Parallel.Strategies
-- System for system()
import System (system)

-- Main parameters
-- Screen size
size :: Int
size = 1000
-- Reflections and refractions depth
depth :: Int
depth = 10
-- Scene 
scene :: Scene
scene = Scene(Camera(0.0,0.0,-100.0) (100,100)) ((0.0,0.0,0.0)) 
      ([
      -- Predifined fractals, from 0 to 10 depth of recursion. 0 - 1 object .. 10 - 2.8 million objects. The size of scene objects count affects memory usage.
      -- objects 0 50 (0.0, 20.0, 130.0) 
      -- objects 1 27 (0.0, 20.0, 130.0)
      -- objects 2 9 (0.0, 20.0, 130.0)
      -- objects 3 3.5 (0.0, 20.0, 130.0) 
      -- objects 4 1.3 (0.0, 20.0, 130.0) 
       objects 5 0.50 (0.0, 20.0, 130.0)
      -- objects 6 0.22 (0.0, 20.0, 130.0)
      -- objects 7 0.09 (0.0, 20.0, 130.0)
      -- objects 9 0.013 (0.0, 20.0, 130.0)
      -- objects 10 0.0052 (0.0, 20.0, 130.0)
       ]
       ++
      -- Nice semi-background plane with cool texture (defined in RayTrace.hs)
       [Data (Plane (0.0,1.0,-0.1,1000.0), (Texture (Bitmap textureBluePlane) 0.3 60 0.0 0.0 ))]
      )
      ([
       -- Default lightning
        (PointLight (30.0,0.0,-300.0) (0.5,0.5,0.5) ),
	(PointLight (-30.0,0.0,-300.0) (0.5,0.5,0.5) )
       ])
-- /Main parameters

-- Main function. Two possibilities: mainPPM or mainNorm, comments below
main :: IO ()
main = mainPPM
--main = mainNorm

-- Output using PPM file and EOG
-- Writes output to /tmp/ folder
mainPPM :: IO ()
mainPPM = 
    do 
       print $ "Objects count: " ++ show (countObjects scene)
       
       savePPM (size,size) (generateAllPoints size size depth scene)

-- Output using window (direct)
-- Hint: at first it opens window, then generates ouput, then displays it. (window looks like crashed, but thats ok)
mainNorm :: IO()
mainNorm = 
    do 
       -- Default block size
       let block = 64

       w <- openWindow "Juozas Kaziukenas" (size,size)

       dlist <- GL.defineNewList GL.CompileAndExecute $ do GL.renderPrimitive GL.Points $ generateBlocks block depth scene (size,size)
       GL.clear [GL.ColorBuffer]

       drawInWindow w (Graphic $ GL.callList dlist)

--       GLFW.swapBuffers

       spaceClose w 

-- ########################
-- #######Auxiliary########
-- ########################

-- Function used for window closing
spaceClose :: Window -> IO ()
spaceClose w = do k <- getKey w
                  if (k==' ' || k == '\x0') then closeWindow w
                                            else spaceClose w

-- Color
convertColor :: RayTrace.Color -> GL.Color3 Float
convertColor (x,y,z) = GL.Color3 (doubleToFloat x) (doubleToFloat y) (doubleToFloat z)

-- Objects generation
objects :: Int -> Double -> Point3D -> ObjectsTree
objects x size coord = objects' 0 (x+1) size coord
  where
    -- Face (1 - left, 2 - top, 3 - right, 4 - bottom, 5, 6- others) used for reducing objects count in space
    objects' :: Int -> Int -> Double -> Point3D -> ObjectsTree
    objects' _ 0 _ _ = Empty
    objects' face 1 size point = 
        Data (Sphere (sizeX size 0) point, (Texture (Solid (
            (if x `mod` 3 == 1 || x `mod` 3 == 2 then 1.0 else 0.0),
            (if x `mod` 3 == 2 || x `mod` 3 == 0 then 1.0 else 0.0), 
            0.0)) 0.0 60 0.0 0.0 ))

    objects' face (x+1) size point = Node ((Sphere (sizeAll size x) point), [
         
        Data (Sphere (sizeX size x) point, (Texture (Solid (
            (if x `mod` 3 == 1 || x `mod` 3 == 2 then 1.0 else 0.0),
            (if x `mod` 3 == 2 || x `mod` 3 == 0 then 1.0 else 0.0), 
            0.0)) 0.4 30 0.0 0.0 )), 

        (if face /= 1 then objects' 3 x size (rotate (pi/3+3*(pi/2)) (pi - pi/3) (distanceX size (x-1)) point) else Empty),
        (if face /= 3 then objects' 1 x size (rotate (pi/3+pi/2) (pi/3)  (distanceX size (x-1)) point) else Empty),
        (if face /= 4 then objects' 2 x size (rotate (pi/3+pi) (pi/2)  (distanceX size (x-1)) point) else Empty),
        (if face /= 2 then objects' 4 x size (rotate (pi/3) (pi - pi/3)  (distanceX size (x-1)) point) else Empty),
        (if face /= 6 then objects' 5 x size (rotate (pi) (pi*0.97)  (distanceX size (x-1)) point) else Empty)

      ])

-- Rotate point around speher, idea: http://en.wikipedia.org/wiki/Spherical_coordinates
rotate :: Double -> Double -> Double -> Point3D -> Point3D
rotate theta sigma radius (x,y,z) = (x + (radius * cos(theta) * sin(sigma)),  y + (radius * sin(theta) * sin(sigma)), z + (radius * cos(sigma)))

-- Gets size of sphere
sizeX :: Double -> Int -> Double
sizeX size x = size*((2.5)^(x))
-- Gets size of spheres inside sphere (for trees)
sizeAll :: Double -> Int -> Double
sizeAll size x = sizeX size (x) + size' x
  where 
   size' 0 = 0
   size' (x+1) = 2*sizeX size x + size' x
-- Distance from point to another sphere center point
distanceX :: Double -> Int -> Double
distanceX size x = sizeX size x + sizeX size (x+1)

-- Packets generation
generateAllPoints :: Int -> Int -> Int -> Scene -> [RayTrace.Color]
generateAllPoints width height d scene = 
  concat $ parMap rnf (\y -> map (\x -> rayTrace d (width,height) scene (x,height-y)) [0..height-1]) [0..width-1]

generateBlocks :: Int -> Int -> Scene -> Resolution -> IO ()
generateBlocks size d scene (width,height) = 
 let xblocks = width `div` size + 1
     yblocks = height `div` size + 1
     blocks  = concat $ map (\x -> map (\y -> (x*size,y*size) ) [0..yblocks-1] ) [0..xblocks-1]
     pixels  = parMap rnf (\(x,y) -> generatePacket x y (x+size) (y+size) width height d scene) blocks
 in
  do
   mapM_ (\pix -> mapM_ (\(color, (x,y)) -> do GL.color (convertColor color) >> GL.vertex (vertex3 (fromIntegral x) (fromIntegral y) 0)) pix) pixels

generatePacket :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Scene -> [(RayTrace.Color, Point)]
generatePacket x0 y0 x1 y1 width height d scene = 
 map (\(x,y) -> (rayTrace d (width,height) scene (x,y) ,(x,height-y))) [(x,y) | x <- [x0..x1], y <- [y0..y1]]
-- /Packets generation

-- Converts Float to Double
floatToDouble :: Float -> Double
floatToDouble = (uncurry encodeFloat).decodeFloat
-- Converts Double to Float
doubleToFloat :: Double -> Float
doubleToFloat = (uncurry encodeFloat).decodeFloat

createPPM :: Resolution -> [RayTrace.Color] -> String
createPPM (w,h) colors =  "P3\n" ++  show w ++ " " ++  show h ++ "\n255\n" ++ stringify
                where stringify = concat $ map showC colors
                      showC (r,g,b) = show (round (r*255+0.5)) ++  " " ++ show (round (g*255+0.5)) ++ " " ++ show (round (b*255+0.5)) ++ " "

savePPM :: Resolution -> [RayTrace.Color] -> IO ()
savePPM res colors = do
  let file = "/tmp/_render_0820151"

  system ("rm -f " ++ file)

  h <- openFile file WriteMode

  hSetBuffering h (BlockBuffering (Just 4096))

  hPutStr h (createPPM res colors)

  hClose h

  system ("eog -n " ++ file)

  return ()

-- #Debug

countObjects :: Scene -> Int 
countObjects (Scene _ _ o _) = foldr (+) 0 (map countObjects' o)
 where
  countObjects' :: ObjectsTree -> Int
  countObjects' Empty = 0
  countObjects' (Data _) = 1
  countObjects' (Node (_, leaf)) = countObjects'' leaf
    where
      countObjects'' [] = 0
      countObjects'' (n:ns) = countObjects' n + countObjects'' ns

printTree :: ObjectsTree -> String
printTree Empty = "*"
printTree (Data (Sphere rad (x,y,z), _)) =  "data " ++ (show rad) ++ " (" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "),"
printTree (Node ((Sphere rad (x,y,z)), leaf)) = "(object " ++ (show rad) ++ " (" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show z) ++ "), " ++ "[" ++ printTree' leaf ++ "])"
  where
    printTree' [] = "-"
    printTree' (n:ns) = printTree n ++ printTree' ns
