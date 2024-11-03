import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

-- Advanced Data Types
-- The same strategy was used for both the adj list and array: get all cities, get adjancencies for each, update data
type AdjList = [(City, [(City, Distance)])]
generateAdjList :: RoadMap -> AdjList -- Function receives a RoadMap and returns the AdjList data structure
generateAdjList roadMap = map buildAdjacency (cities roadMap)
    where
        buildAdjacency city = (city, adjacent roadMap city) -- Uses function to generate row

type AdjMatrix =  Data.Array.Array (Int, Int) (Maybe Distance)
cityIndex :: String -> Int -- Necessary to get index out of city
cityIndex index = read index
generateAdjMatrix :: RoadMap -> AdjMatrix -- Function receives a RoadMap and returns the AdjMatrix data structure
generateAdjMatrix roadMap = foldr updateArray adjArray cityList
    where
        -- 1) Setup Empty Array -> every cell contains Nothing
        cityList = cities roadMap
        arrayLength = length cityList
        arrayBounds = ((0, 0), (arrayLength - 1, arrayLength - 1))
        adjArray = Data.Array.array arrayBounds [((i, j), Nothing) | i <- [0..arrayLength-1], j <- [0..arrayLength-1]]
        -- 2) Transverse the adjacency list to update the array
        updateArray city acc = foldr (updateRow city) acc (adjacent roadMap city)
        updateRow city (adjCity, dist) acc = acc Data.Array.//  [((cityIndex city, cityIndex adjCity), Just dist)]

-- Functions

cities :: RoadMap -> [City]
cities roadMap = foldr addIfNotDuplicate [] roadMap -- Transverse the list and accumulate non-duplicate cities. 'nub' was not used because, presumably, it would transverse the list twice.
    where
        addIfNotDuplicate (city1, city2, _) acc = checkDuplicate city1 (checkDuplicate city2 acc)
        checkDuplicate city acc
            | city `elem` acc = acc
            | otherwise = city : acc

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap cityA cityB = foldr isAdjacent False roadMap -- Transverse the list and check if two cities are adjacent. 'any' could have been used as well.
    where
        isAdjacent (c1, c2, _) acc = ((c1 == cityA && c2 == cityB) || (c1 == cityB && c2 == cityA)) || acc

distance :: RoadMap -> City -> City -> Maybe Distance
distance roadMap cityA cityB = foldr isAdjacent Nothing roadMap -- Transverse the list and check the distance between two cities. 'find' could have been used as well.
    where
        isAdjacent (c1, c2, dist) acc
            | (c1 == cityA && c2 == cityB) || (c1 == cityB && c2 == cityA) = Just dist
            | otherwise = acc

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent roadMap city = foldr isAdjacent [] roadMap -- Transverse the list and check the distance for all cities connected to one other city. 'filter' could have been used as well.
    where
        isAdjacent (c1, c2, dist) acc
            | c1 == city = (c2, dist) : acc
            | c2 == city = (c1, dist) : acc -- The city may be in the first or second position
            | otherwise  = acc

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales roadmap
    | not (5==5) = [] -- True, leave as is
    | otherwise = let cityList = cities roadmap
                      cityIndices = Data.Array.listArray (0, listLength - 1) cityList
                      infinity = maxBound :: Int -- Infinity: couldn't get Nothing to work to represent this idea
                      distances = Data.Array.array ((0, 0), (listLength - 1, listLength - 1))
                          [((i, j), maybe infinity id (distance roadmap (cityIndices Data.Array.! i) (cityIndices Data.Array.! j)))
                          | i <- [0..listLength-1], j <- [0..listLength-1]] -- Setting up Adjacency Table
                      listLength = length cityList
                      fullSet = (1 `Data.Bits.shiftL` listLength) - 1 :: Int -- Bitmask representing all cities
                      -- Recursion is started by filling in the dynamic table
                      tableBounds = ((0,0), (fullSet, listLength-1))
                      dynamicTable = Data.Array.array tableBounds [((subset, city), calculateSubset subset city) | subset <- [0..fullSet], city <- [0..listLength-1]] :: Data.Array.Array (Int, Int) Distance    
                      calculateSubset subset city
                          | subset == (1 `Data.Bits.shiftL` city) = if city == 0 then 0 else infinity -- Base Case or unreachable set
                          | Data.Bits.testBit subset city = minimum [dynamicTable Data.Array.! (Data.Bits.clearBit subset city :: Int, previousCity) -- Calculate subset ending in previousCity
                              + distances Data.Array.! (previousCity, city) -- Add distance to current city
                              | previousCity <- [0..listLength-1], previousCity /= city, Data.Bits.testBit subset previousCity] -- Previous City cannot be city itself and must be in subset
                          | otherwise = infinity -- Invalid Subset
                      
                      -- Extracting the best path
                      finalCost = minimum [dynamicTable Data.Array.! (fullSet, city) + distances Data.Array.! (city, 0) | city <- [1..listLength-1]]
                      buildPath set city path
                          | set == (1 `Data.Bits.shiftL` city) = reverse (cityIndices Data.Array.! city : path)
                          | otherwise = let previousCity = Data.List.minimumBy (\i j -> compare (dynamicTable Data.Array.! (Data.Bits.clearBit set city, i) + distances Data.Array.! (i, city))
                                              (dynamicTable Data.Array.! (Data.Bits.clearBit set city, j) + distances Data.Array.! (j, city)))
                                              [i | i <- [0..listLength-1], i /= city, Data.Bits.testBit set i]
                              in buildPath (Data.Bits.clearBit set city) previousCity (cityIndices Data.Array.! city : path)
                      
                  in if finalCost == infinity then [] else buildPath fullSet 0 []

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]