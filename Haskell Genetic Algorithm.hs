{-Louis Mitchener, Haskell Slide-}

{-for chart visualisation run the line 'stack install GoogleChart-0.2' into the command prompt if using Stack, or install the Google Charts Package http://hackage.haskell.org/package/GoogleChart-0.2 if manual install of packages is required-}

import Data.List
import System.Random  
import Graphics.Google.Chart



----part 1----

type Point = (Float,Float)

velocity :: Float -> Float -> Float -- calculates velocity at any given point     
velocity y1 y2 = sqrt(2*g*(y1-y2))
  where 
    g = 9.81

distance :: Point -> Point -> Float -- calculates distance between 2 connected points
distance (x1, y1) (x2, y2) = sqrt((y2-y1)**2 + (x2-x1)**2) 

time :: Float -> Float -> Float -> Float -- calculates time between 2 connected points
time x y dist = dist/avg_v
  where 
    avg_v = (y+x)/2

arrange_in_pairs :: Point -> Point -> [Point] -> [(Point, Point)] -- creates pairs of points forming the surface of the slide
arrange_in_pairs start end x = [(start,(head x))]++zip x (tail x)++[((last x),end)]

total_time :: Point -> Point -> [Point] -> Float --calculates total time taken to traverse any given slide
total_time start end x = sum timeList
  where
    velocityList = map find_velocity [(q,w) | q <- [snd start], w <- map snd ([start]++x++[end])]
    distanceList = map find_distance (arrange_in_pairs start end x)
    pairedVelocityList = zip velocityList (tail velocityList)
    velocityDistanceList = zip pairedVelocityList distanceList
    timeList = map find_time velocityDistanceList

find_velocity :: Point -> Float -- maps velocity list to a format readable by the function velocity
find_velocity (x,y) = velocity x y

find_distance :: (Point,Point) -> Float -- maps distance list to a format readable by the function distance
find_distance (x,y) = distance x y

find_time :: ((Point),Float) -> Float -- maps list of distances and times to a format readable by the function time
find_time ((x, y),dist) = time x y dist



----part 2----

type Candidate = (Point,Point,[Point],Float)

make_candidates :: Point -> Point -> [[Point]] -> [Candidate] -- generates a list of one or more candidates from a start and end point and one or more lists of supporting points
make_candidates start end x
    |tail x ==[]=[(start,end,(head x),time)]
    |otherwise = [(start,end,(head x),time)]++ make_candidates start end (tail x)
  where
    time = total_time start end (head x)

sort_by_time :: [Candidate] -> [Candidate] -- sorts the list of candidates from fastest time to slowest time
sort_by_time x = sortBy candidate_sort x

candidate_sort :: Candidate -> Candidate -> Ordering -- gives sorting parameters which ensures the candidates are sorted by time only
candidate_sort (x,y,(w:ws),z) (x1,y1,(w1:w1s),z1) = compare z z1

candidate_to_string :: Candidate -> String --converts a candidate to a string value which is easier to read
candidate_to_string (start,end,x,time) = first ++ mid ++ last ++ "Time: " ++ show time
  where
    first = point_to_string start ++ "\n"
    last = point_to_string end ++ "\n"
    mid = concat (intersperse "\n" (map point_to_string x)) ++ "\n"
   
point_to_string:: Point -> String --converts a point to a string value which is easier to read
point_to_string (x,y) = show x ++ " " ++ show y

divide_list :: [Float] -> [Float] -> [[Point]] --combines two lists of numbers into a list of points
divide_list x y
    | y == [] = []
    | otherwise = [zip x y] ++ divide_list x (drop (length x) y)



----part 3----

random_list :: Float -> (Float,Float) -> StdGen -> ([Float],StdGen) --generates a set number of random numbers between two values, requires a random number generator
random_list 0 _ gen = ([], gen)
random_list n minmax gen = ((r:rs), g2)
  where
    ( r , g) = randomR minmax gen
    ( rs, g2) = random_list (n-1) minmax g

create_random_candidates :: Int -> Point -> Point -> [Float] -> (Float,Float) -> StdGen -> ([Candidate], StdGen) -- takes number of candidates, start point, end point, x vals of supporting points, min and max values of number generator, and a random number generator and produces a list of candidates and random number generator
create_random_candidates n start end xs (min,max) gen = (candidates,gen1)
  where
    midnumber = (length xs)*n
    (randomlist,gen1) = random_list (realToFrac midnumber) (min,max) gen
    middlepoints = divide_list xs randomlist
    candidates = make_candidates start end middlepoints


    
----part 4----

crossover :: [Candidate] -> Int -> StdGen -> ([Candidate], StdGen) -- selects up to n elements of a list of candidates and creates all possible pairs of candidates up to the nth candidate, then applies the cross pairs to each of them. The output is the original list of candidates plus the new list of candidates produced by cross_pairs
crossover cs n g = (cs ++ cs_new, g1)
  where
    pairs = [(( cs !! c1), (cs !! c2)) | c1 <- [0..(n-1)], c2 <- [(c1+1)..(n-1)]]
    (cs_new, g1) = cross_pairs pairs g

cross_pairs :: [(Candidate, Candidate)] -> StdGen -> ([Candidate], StdGen) -- takes a list of two tuples of candidates and applies the crosspair function to each two tuple, each pair gives one new point which is a combination of each candidate in the two tuple. 
cross_pairs [] g = ([], g)
cross_pairs (cp:cps) g = (c:cs, g2)
  where
    (c, g1) = cross_pair cp g
    (cs, g2) = cross_pairs cps g1

cross_pair :: (Candidate, Candidate) -> StdGen -> (Candidate, StdGen) -- takes a pair of candidates and outputes a single candidate, selecting randomly the middle points from the two input candidates and combining them into the new candidate
cross_pair (( s, e, ps1, _ ), (_, _, ps2, _)) g = (( s, e, ps, t ), g1)
  where
    (ps, g1) = cross_supp ps1 ps2 g
    t = total_time s e ps

cross_supp :: [Point] -> [Point] -> StdGen -> ([Point], StdGen) -- combines two lists of points into one, randomly selecting from each list which point to use in the output list
cross_supp [] [] g = ([], g)
cross_supp (c1:cs1) (c2:cs2) g = (( if r < 0.5 then c1 else c2) : xs, g2)
  where
    ( r , g1) = randomR (0 :: Float, 1 :: Float) g
    (xs, g2) = cross_supp cs1 cs2 g1

mutation :: Int -> Int -> (Float, Float) -> [Candidate] -> StdGen -> ([Candidate], StdGen) -- takes the number of candidates to be mutated, the number of positions to be mutated, the min max range that y values can be, a list of candidates, and a random number generator and returns a new list of old and new candidates and a new random number generator
mutation n positions (min,max) cs rnd = (cs++new_list,new_rnd)
  where
    (new_list,new_rnd) = mutate_recurse n positions (min,max) cs rnd

mutate_recurse :: Int -> Int -> (Float, Float) -> [Candidate] -> StdGen -> ([Candidate], StdGen)-- recursively mutates n candidates in list of candidates
mutate_recurse n positions (min,max) cs rnd
    | n == 1 = ([current_candidate],new_rnd)
    | otherwise = ([current_candidate]++ next_iteration ,next_rnd)
  where
    (current_candidate,new_rnd) = mutate_candidate positions (min,max) (cs!!(n-1)) rnd
    (next_iteration,next_rnd) = mutate_recurse (n-1) positions (min,max) cs new_rnd

mutate_candidate :: Int -> (Float, Float) -> Candidate -> StdGen -> (Candidate,StdGen) --mutates a single candidate
mutate_candidate positions (min,max) (x1,y1,x,time) rnd = ((x1,y1,points_list,new_time),new_gen)
  where
    (points_list,gen_list) = unzip(mutate_points (min,max) x rnd)
    new_gen = last(gen_list)
    new_time = total_time x1 y1 points_list

mutate_points :: (Float,Float) -> [Point] -> StdGen -> [(Point,StdGen)] --mutates a single point within a given candidate
mutate_points (min,max) ((x1,y1):xs) rnd
    |(xs==[]) && ((y1-10)<y2 && y2<(y1+10)) = [((x1, y2),new_rnd)]
    |(y1-10)<y2 && y2<(y1+10) = [((x1, y2),new_rnd)] ++ mutate_points (min,max) xs new_rnd
    |otherwise = mutate_points (min,max) ((x1,y1):xs) new_rnd
  where
    (y2,new_rnd) = randomR (min,max) rnd 



----part 5----

{-for chart visualisation run the line 'stack install GoogleChart-0.2' into the command prompt if using Stack, 
or install the Google Charts Package http://hackage.haskell.org/package/GoogleChart-0.2 if manual install of packages is required-}


make_chart :: Candidate -> String -- takes a candidate and produces a url leading to a graph of the candidate (requires Google Charts Package)
make_chart (start, end, xs, time) = x
  where
    tims = "Time = "++ show time
    points = [snd start] ++ map snd xs ++ [snd end]
    x = show $ chartURL $ setSize 547 548 $ setTitle "Slide" $ setData (encodeDataText [points]) $ setLegend [tims] $ newLineChart


----part 6----


create_slide :: Point -> Point -> [Float] -> Int -> Int -> Int -> Int -> Int -> Point -> Int -> IO() -- Takes a start point, end point, list of support x values, number of iterations, number of candidates, number of crossbreeds per pass, number of mutations per pass, number of values to be mutated within each candidate, a min max range for y values, and a seed for a random number generator, and outputs the fastest slide generated and a link to a graph of it
create_slide start end support nit ncand nbreed nmut npos minmax seed = putStr(winner_str ++"\n"++"visit URL for graph: \n"++ winner_chart++"\n")
  where
    (init_candidates,new_seed) = create_random_candidates ncand start end support minmax (mkStdGen seed)
    starting_candidates = sort_by_time init_candidates
    final_candidates = improve_slide starting_candidates nit ncand nbreed nmut npos minmax new_seed
    winner = head final_candidates
    winner_chart = make_chart winner
    winner_str = candidate_to_string winner

improve_slide :: [Candidate] -> Int -> Int -> Int -> Int ->Int -> Point -> StdGen -> [Candidate]-- genetic algorithm to improve the slide time of a population of slides
improve_slide candidates nit ncand nbreed nmut npos minmax rnd
    |nit == 0 = candidates
    |otherwise = improve_slide filtered_candidates (nit - 1) ncand nbreed nmut npos minmax rnd2
  where
    (crossover_candidates, rnd1) = crossover candidates nbreed rnd
    (mutate_candidates, rnd2) = mutation nmut npos minmax candidates rnd1
    filtered_candidates = take ncand (remove_duplicates(sort_by_time(crossover_candidates++mutate_candidates)))

remove_duplicates :: [Candidate] -> [Candidate] -- takes a sorted list of candidates and removes duplicates
remove_duplicates (x:xs)
    |xs ==[]= [x]
    |x == head xs =  remove_duplicates xs
    |otherwise = [x] ++  remove_duplicates xs