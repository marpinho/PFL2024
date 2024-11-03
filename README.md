# PFL TP1 2024 2025

## 1- Contributors
| Student | Student ID | Contribution |
| -- | -- | -- |
| Guilherme António Cerqueira Magalhães | up202005285 | 50% |
| Margarida Alves Pinho | up201704599 | 50% |

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
