--import qualified Data.List
import qualified Data.Array
--import qualified Data.Bits

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
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]