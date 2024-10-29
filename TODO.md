# Trabalho Prático 1

## Submission

- 20:00 of the 3rd of November
- PFL_TP1_G7_09.zip
  - TP1.hs
  - README.pdf
- Must not contain any other directories or files
- Both files must be at the archive root level

## Source code file

1. [ ] Must run under GHC, version 9.10.1.
2. [ ] The datatype definitions and function signatures of the initial file must not be modified.
3. [ ] If you do not implement a version of a function asked in the assignment, leave it as is (i.e. as undefined).
4. [ ] Must be properly commented: in each function include the type declaration, description of the goal and the meaning of the arguments.

## README file

1. [ ] Group members.
2. [ ] Percentage of the contribution of each member.
3. [ ] Brief description the tasks each one performed.
4. [ ] Explanation for the function shortestPath, including why certain auxiliary data structures were selected, and a description of the algorithm(s) used.
5. [ ] Explanation for the function travelSales, including why certain auxiliary data structures were selected, and a description of the algorithm(s) used.

## Assignment

The goals of this project are to define and use appropriate data types for a graph representing a country, composed of a set of interconnected cities.

- Auxiliary types:
  - type City = String
  - type Distance = Int  (Assume as integer numbers)
  - type Path = [City]

- The type for graphs used as input: type RoadMap = [(City,City,Distance)].
  - vertices: cities.
  - each edge: tuple with the name of the cities it connects and the distance connecting them
  - the graph is undirected.

- To achieve more efficient solutions use the data structures from the GHC modules that can be imported to convert the graph to a more convenient representation.
- Any other data structure needed to improve efficiency (such as a heap) must be implemented by the students.

- Graph representations:
  1. Adjacency list representation
    - type AdjList = [(City,[(City,Distance)]]
    - better with sparce maps, when there are few roads
  2. Adjacency matrix representation
    - type AdjMatrix = Data.Array.Array (Int,Int) (Maybe Distance)
    - better with dense maps, when there is a large number of roads
  3. Pointer representation
    - data AdjPointers = Place City [(AdjPointers, Distance)]
    - very space efficient but its operations are a bit
more complicated and time inefficient.

## Functions

1. [ ] cities :: RoadMap -> [City], returns all the cities in the graph.
2. [ ] areAdjacent :: RoadMap -> City -> City -> Bool, returns a boolean indicating whether two cities are linked directly.
3. distance :: RoadMap -> City -> City -> Maybe Distance, returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise.
4. [ ] adjacent :: RoadMap -> City -> [(City,Distance)], returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them.
5. [ ] pathDistance :: RoadMap -> Path -> Maybe Distance, returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing.
6. [ ] rome :: RoadMap -> [City], returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree).
7. [ ] isStronglyConnected :: RoadMap -> Bool, returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city).
8. [ ] shortestPath :: RoadMap -> City -> City -> [Path], computes all shortest paths [RL99, BG20] connecting the two cities given as input. Note that there may be more than one path with the same total distance. If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c].
9. [ ] travelSales :: RoadMap -> Path, given a roadmap, returns a solution of the Traveling Salesman Problem (TSP). In this problem, a traveling salesperson has to visit each city exactly once and come back to the starting town. The problem is to find the shortest route, that is, the route whose total distance is minimum. This problem has a known solution using dynamic programming [RL99]. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. Note that the roadmap might not be a complete graph (i.e. a graph where all vertices are connected to all other vertices). If the graph does not have a TSP path, then return an empty list.