# PFL TP1 2024 2025

## 1- Contributors
| Student | Student ID | Contribution | Exercises |
| -- | -- | -- | -- |
| Guilherme António Cerqueira Magalhães | up202005285 | 50% | 1,2,3,4,9 |
| Margarida Alves Pinho | up201704599 | 50% | 5,6,7,8 |

## 2- Implementation of shortestPath function

## 3- Implementation of travelSales function
### 3.1- Traveling Salesman Problem
The dynamic programming solution to the Traveling Salesman Problem (TSP) is the **Held-Karp Algorithm**. This algorithm ensures that we get to the optimal solution and, given that TSP is a NP-Hard problem, has exponential time complexity: O(2^n*n^2). It is not as inneficient as a brute force O(n!) solution because it stores intermediate results to avoid redundant calculations.

*Note: If two vertices are not connected, we can assume distance between the two is infinite. Due to the Problem Definition we represented it as 'Nothing'.*

##### State Definition
The key insight that enables this algorithm is that the cost of 'solving TSP' for a subset of the graph is more important than the exact path taken inside the subset. This means that subsets of cities can be treated as 'composite cities', with an associate cost. For example:
-  Let's say we have a subset of cities {A, B, C, D} and want to end at D.
-  We know that all possible routes were evaluated (A->B->C->D, B->A->C->D) and reached the conclusion that A->B->C->D is the optimal path ending at D.
-  Now let's say we want to reach E from D. We just add the cost from previous point to E-D. 

Based on this we can define the program's state as:
1. A set of visited cities
2. The last visited city

##### Recursive Step

The goal of the recursive step is to find which city is 'cheapest' to add to the subset, assuming this city is visited last. This means that we will iterate over all cities not already in the subset. In order to calculate total cost for the new city we will have to:
- calculate the transversal cost for the subset, ending in every city included in it
- add to this cost the travel to the new city
 
The distance of a city to itself has cost 0 (first step). In the last step we will calculate the cost to loop back around to the path origin.

### 3.2- Implementation
We implemented TSP in the travelSales function.

#### States and the Dynamic Table
As previously mentioned each state can be represented as:
- A Set of visited cities, which can be represented using a bitmask.
- The current city, which can be represented as an integer

The use of bitmasking in this solution is very space efficient, as it occupies as little space as possible (each city is 1 bit). The following code snippets can be used to facilitate interaction:
- (1 \Data.Bits.shiftL` city)` to represent a set with only 1 city (1 bit in the cities position)
- Data.Bits.testBit to check if a city is in a set
- Data.Bits.clearBit to remove a city from a set

States can be stored in a 2D Array, where the subset bitmask is one dimension and the current city integer is another. This bitmask can be interpreted as an integer.

#### Recursion
The recursive function is started when populating the table. Each cell will be populated with either infinity or a value acquired through finding a valid path. This happens in the calculateSubset function.

The base case is the subset occupied by the first city only. This has a distance of 0. Any invalid subset (doesn't contain origin city or end city) is given a distance of infinity.

*Note: Infinity is defined as 'infinity = (maxBound :: Int) `div` 1000'. This is because using infinity directly caused an overflow. This is an **obviously wrong way of representing it**, that will work in most cases.*

Distances to the new city are retrieved using adjacencyList. This array once is calculated using the distances function, replacing Nothing with infinity. **It does not follow the type definition provided in the project definition, to avoid issues with Nothing**. This list enables us to calculate results only once and is especially efficient with dense maps.

#### Return value
The finalCost is calculated by adding the cost to return to the origin. The subset with the better cost is then selected. The path is reverse engineered using buildPath to explore the dynamic table.