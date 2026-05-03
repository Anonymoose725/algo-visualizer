# algo-visualizer

My Haskell backend that generates step-by-step execution traces of sorting algorithms, served as JSON over HTTP.

Built as the backend for [algo-visualizer-frontend](https://algo-visualizer-frontend-six.vercel.app): a full-stack algorithm visualizer. (See the [repository](https://github.com/Anonymoose725/algo-visualizer-frontend))

---

## What It Does

Each endpoint returns intermediate steps in an algorithm - typically elements are compared, and related data at that moment is returned. This generally includes indices, a full array state at that moment, etc. The frontend uses these trace datapoints to animate the algorithm step-by-step.

---

## Stack

| Layer | Technology | Role |
|---|---|---|
| Language | Haskell - GHC | Core logic and API |
| API framework | [Servant](https://docs.servant.dev) | Type-safe HTTP routing with a unique feature: the web API is defined as a Haskell type, so the compiler rejects handlers that return the wrong data |
| HTTP server | [Warp](https://hackage.haskell.org/package/warp) | Runs the server and handles network I/O |
| JSON | [Aeson](https://hackage.haskell.org/package/aeson) | Serializes Haskell types to JSON automatically via `Generic` |
| Build tool | [Cabal](https://cabal.readthedocs.io) | Dependency management and source compilation |

---

## Running locally

**Prerequisites:** GHC and Cabal via [GHCup](https://www.haskell.org/ghcup/), see instructions for your specific hardware

```bash
git clone https://github.com/Anonymoose725/algo-visualizer
cd algo-visualizer
cabal build
cabal run
```

Server starts on `http://localhost:8080`. Change if necessary!

---

## API

### `GET /sort/bubble`

Runs bubble sort and returns every comparison as a step trace.

**Query Parameters**

| Parameter | Type | Example |
|---|---|---|
| `input` | Comma separated integers | `3,1,4,2` |

**Example Request**

```bash
curl "http://localhost:8080/sort/bubble?input=3,1,4,2"
```

**Example Response**

```json
[
  {
    "stepNumber": 1,
    "currentState": [3, 1, 4, 2],
    "comparing": [3, 1],
    "comparingIndices": [0, 1]
  },
  {
    "stepNumber": 2,
    "currentState": [1, 3, 4, 2],
    "comparing": [3, 4],
    "comparingIndices": [1, 2]
  }
]
```

**Response Fields**

| Field | Type | Description |
|---|---|---|
| `stepNumber` | Int | Step counter, incrementing across passes |
| `currentState` | [Int] | Full array at this instant in the algorithm |
| `comparing` | [Int, Int] | The two values being compared |
| `comparingIndices` | [Int, Int] | The indices of those values in `currentState` |

---

### `GET /sort/merge`

The same interface as `/sort/bubble`, runs merge sort instead!

```bash
curl "http://localhost:8080/sort/merge?input=3,1,4,2"
```

---

## Project structure

```
app/
├── Main.hs                 — starts Warp server, reads PORT env variable (8080 otherwise)
├── API.hs                  — Servant API type definition and request handlers
├── Types.hs                — shared data types (Step, BTree, etc)
└── Algorithms/
    ├── Sorting.hs          — bubble sort, merge sort, step trace generation
    ├── Trees.hs            — implements related functions for Tree datatypes
    └── BST.hs              - binary search tree algorithms (build, insert, search)
```

---

## Design

**Pure algorithm logic** — sorting functions have no knowledge of HTTP or JSON. They are pure Haskell functions `[Int] -> [Step]` that happen to be called by web handlers. This separation means algorithms are independently testable in GHCi. Algorithms are modular and can be added, updated, maintained with minimal effort and headache. Only our main function remains impure as it handles IO.

**Type-safe API** — Servant defines the API as a type:
```haskell
type AlgoAPI
  = "sort" :> "bubble" :> QueryParam "input" String :> Get '[JSON] [Step]
  :<|> "sort" :> "merge"  :> QueryParam "input" String :> Get '[JSON] [Step]
```
The compiler makes sure that every handler matches this definition. A handler returning the wrong type will not compile.

**Step indices** — `comparingIndices` stores the actual array positions of compared elements rather than their values. This avoids ambiguity when duplicate values appear in the input. Less restriction on input provides a more robust system.

**Graph Steps** - include auxillary information for graphs, and by extension trees. Having seperate step-types for different abstract datatypes gives room for JSON efficiency and only requesting necessary fields.

---

## Roadmap / Changes

**Improvements**

1. Add Action datatype to Step so algorithms do not need to infer the type of action at each comparison
    ```haskell
    data Action
        = Compare (Int, Int)
        | Swap Int Int
        | Write Int Int
    ```
    So Step becomes
    ```haskell
    data Step = Step
     { stepNumber :: Int,
       currentState :: [Int],
       action :: Action,
       partitionInfo :: Maybe PartitionInfo
     }
    ```
2. More algorithms!
    - quicksort (with pivot highlighting)
    - heap sort (tree + array dual view)
    - binary search trees (done!)
    - redblack trees + rotations
    - dijkstra's shortest path + graphs

3. Graph building from adjcency list, received from frontend (user builds! from drag-and-drop)

4. Data.Vector upgrade, only if performance requires it

5. Export/import prebuilt data structures
