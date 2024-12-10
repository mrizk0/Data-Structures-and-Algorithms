#lang dssl2

import cons
import sbox_hash


# A Vertex is a natural number.
let Vertex? = nat?

# A VertexList is either
#  - None, or
#  - cons(v, vs), where v is a Vertex and vs is a VertexList
let VertexList? = Cons.ListC[Vertex?]

# A Weight is a real number. (It’s a number, but it’s neither infinite
# nor not-a-number.)
let Weight? = AndC(num?, NotC(OrC(inf, -inf, nan)))

# An OptWeight is either
# - a Weight, or
# - None
let OptWeight? = OrC(Weight?, NoneC)

# A WEdge is WEdge(Vertex, Vertex, Weight)
struct WEdge:
    let u: Vertex?
    let v: Vertex?
    let w: Weight?

# A WEdgeList is either
#  - None, or
#  - cons(w, ws), where w is a WEdge and ws is a WEdgeList
let WEdgeList? = Cons.ListC[WEdge?]

# A weighted, undirected graph ADT.
interface WUGRAPH:

    def len(self) -> nat?

    def set_edge(self, u: Vertex?, v: Vertex?, w: OptWeight?) -> NoneC

    def get_edge(self, u: Vertex?, v: Vertex?) -> OptWeight?

    def get_adjacent(self, v: Vertex?) -> VertexList?

    def get_all_edges(self) -> WEdgeList?

class WUGraph (WUGRAPH):
    let size
    let adj
    
    def __init__(self, size: nat?):
        self.size = size
        self.adj = [None; size]
        for i in range(size):
            self.adj[i] = [None; size]
            
    def len(self):
        return self.size
        
    def set_edge(self, u, v, w):
        if num?(w):
            self.adj[u][v] = w
            self.adj[v][u] = w
        if w == None:
            self.adj[u][v] = None
            self.adj[v][u] = None
            
    def get_edge(self, u, v):
        return self.adj[u][v]
        
    def get_adjacent(self, v):
        let adjs = None
        for i in range(len(self)):
            if num?(self.adj[v][i]):
                adjs = cons(i, adjs)
        return adjs
            
    def get_all_edges(self):
        let edges = None
        for i in range(len(self)):
            for j in range(i, len(self)):
                if num?(self.adj[i][j]):
                    edges = cons(WEdge(i, j, self.adj[i][j]), edges)
        return edges



# sort_vertices : ListOf[Vertex] -> ListOf[Vertex]
# Sorts a list of numbers.
def sort_vertices(lst: Cons.list?) -> Cons.list?:
    def vertex_lt?(u, v): return u < v
    return Cons.sort[Vertex?](vertex_lt?, lst)

# sort_edges : ListOf[WEdge] -> ListOf[WEdge]
# Sorts a list of weighted edges, lexicographically
def sort_edges(lst: Cons.list?) -> Cons.list?:
    def edge_lt?(e1, e2):
        return e1.u < e2.u or (e1.u == e2.u and e1.v < e2.v)
    return Cons.sort[WEdge?](edge_lt?, lst)

###
### DFS
###

# dfs : WUGRAPH Vertex [Vertex -> any] -> None
# Performs a depth-first search starting at `start`, applying `f`
# to each vertex once as it is discovered by the search.
def dfs(graph: WUGRAPH!, start: Vertex?, f: FunC[Vertex?, AnyC]) -> NoneC:
    let visited = [False; len(graph)]
    def dfs2(v):
        visited[v] = True
        f(v)
        let adj_cons = graph.get_adjacent(v)
        let adj = Cons.to_vec(adj_cons)
        if adj != None:
            for i in adj:
                if visited[i] == False:
                    dfs2(i)
    dfs2(start)