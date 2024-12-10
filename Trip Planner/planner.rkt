#lang dssl2

import 'project-lib/graph.rkt'
import 'project-lib/dictionaries.rkt'
import 'project-lib/binheap.rkt'

import cons
import sbox_hash

### Basic Types ###

#  - Latitudes and longitudes are numbers:
let Lat?  = num?
let Lon?  = num?

#  - Point-of-interest categories and names are strings:
let Cat?  = str?
let Name? = str?

### Raw Item Types ###

#  - Raw positions are 2-element vectors with a latitude and a longitude
let RawPos? = TupC[Lat?, Lon?]

#  - Raw road segments are 4-element vectors with the latitude and
#    longitude of their first endpoint, then the latitude and longitude
#    of their second endpoint
let RawSeg? = TupC[Lat?, Lon?, Lat?, Lon?]

#  - Raw points-of-interest are 4-element vectors with a latitude, a
#    longitude, a point-of-interest category, and a name
let RawPOI? = TupC[Lat?, Lon?, Cat?, Name?]

### Contract Helpers ###

# ListC[T] is a list of `T`s (linear time):
let ListC = Cons.ListC
# List of unspecified element type (constant time):
let List? = Cons.list?


interface TRIP_PLANNER:

    # Returns the positions of all the points-of-interest that belong to
    # the given category.
    def locate_all(
            self,
            dst_cat:  Cat?           # point-of-interest category
        )   ->        ListC[RawPos?] # positions of the POIs

    # Returns the shortest route, if any, from the given source position
    # to the point-of-interest with the given name.
    def plan_route(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_name: Name?          # name of goal
        )   ->        ListC[RawPos?] # path to goal

    # Finds no more than `n` points-of-interest of the given category
    # nearest to the source position.
    def find_nearby(
            self,
            src_lat:  Lat?,          # starting latitude
            src_lon:  Lon?,          # starting longitude
            dst_cat:  Cat?,          # point-of-interest category
            n:        nat?           # maximum number of results
        )   ->        ListC[RawPOI?] # list of nearby POIs


class TripPlanner (TRIP_PLANNER):
    let map # graph
    let track # dict - coord to vertex
    let rev # (reverse) dict - vertex to coord
    let nseg # num of segs
    let size # num of coords
    let pois # vector of pois
    
    def __init__(self, seg, poi):
        self.nseg = seg.len()
        self.pois = poi
        self.map = WUGraph(self.nseg * 2)
        self.track = HashTable(self.nseg, make_sbox_hash())
        self.rev = HashTable(self.nseg, make_sbox_hash())
        self.size = 0
        for i in seg:
            let start = [i[0],i[1]]
            let end = [i[2],i[3]]
            if self.track.mem?(start) == False:
                self.track.put(start,self.size)
                self.rev.put(self.size,start)
                self.size = self.size + 1
            if self.track.mem?(end) == False:
                self.track.put(end,self.size)
                self.rev.put(self.size,end)
                self.size = self.size + 1
            self.map.set_edge(self.track.get(start),self.track.get(end), dst(start[0],start[1],end[0],end[1]))
        
    def locate_all(self,cat):
        let pos = None
        let fin = [None; self.size]
        for i in self.pois:
            if i[2] == cat and memb(fin,[i[0],i[1]]) == False:
                pos = cons([i[0],i[1]],pos)
                apnd(fin,[i[0],i[1]])
        return pos
        
    def plan_route(self,lat,lon,name):
        let loc = None
        for i in self.pois:
            if i[3] == name:
                loc = i
        if loc == None: return None
        if self.track.mem?([lat,lon]) == False: return None
        let dist = [inf; self.size]
        let pred = [None; self.size]
        let todo = BinHeap(self.size ** 2, λ x, y: dist[x] < dist[y])
        let done = [False; self.size]
        let start = self.track.get([lat,lon])
        dist[start] = 0
        todo.insert(start)
        let m
        while len(todo) > 0:
            m = todo.find_min()
            todo.remove_min()
            if done[m] == False:
                done[m] = True
                let adj = self.map.get_adjacent(m)
                while adj != None:
                    let w = self.map.get_edge(m, adj.data)
                    if (dist[m] + w) < dist[adj.data]:
                        dist[adj.data] = dist[m] + w
                        pred[adj.data] = m
                        todo.insert(adj.data)
                    adj = adj.next
        let route = None
        let end = self.track.get([loc[0],loc[1]])
        route = cons(self.rev.get(end), route)
        while end != start:
            end = pred[end]
            if end == None:
                return None
            route = cons(self.rev.get(end), route)
        return route
        
    def find_nearby(self,lat,lon,cat,n):
        let start = self.track.get([lat,lon])
        let dist = [inf; self.size]
        let pred = [None; self.size]
        dist[start] = 0
        let todo = BinHeap(self.size ** 2, λ x, y: dist[x] < dist[y])
        let done = [False; self.size]
        todo.insert(start)
        let m
        while len(todo) > 0:
            m = todo.find_min()
            todo.remove_min()
            if done[m] == False:
                done[m] = True
                let adj = self.map.get_adjacent(m)
                while adj != None:
                    let w = self.map.get_edge(m, adj.data)
                    if (dist[m] + w) < dist[adj.data]:
                        dist[adj.data] = dist[m] + w
                        pred[adj.data] = m
                        todo.insert(adj.data)
                    adj = adj.next
        let heap = BinHeap(self.size**2, λ x, y: dist[self.track.get([x[0],x[1]])] < dist[self.track.get([y[0],y[1]])])
        let ps = [None; self.size]
        for i in self.pois:
            let x = self.track.get([i[0],i[1]])
            let d = ps[x]
            ps[x] = cons(i, d)
        for i,j in ps:
            if dist[i] != inf:
                let loc = j
                while loc != None:
                    if loc.data[2] == cat:
                        heap.insert(loc.data)
                    loc = loc.next
        let places = None
        while heap.len() > 0 and n > 0:
            let end = heap.find_min()
            places = cons(end, places)
            n = n-1
            heap.remove_min()
        return places
        

def dst(x,y,x1,y1):
    ((x1-x)**2 + (y1-y)**2)**0.5
def memb(self,x):
    for i in self:
        if x == i:
            return True
    return False
def apnd(self,x):
    for i in range(self.len()):
        if self[i] == None:
            self[i] = x
            break

# TESTS
def my_first_example():
    return TripPlanner([[0,0, 0,1], [0,0, 1,0]],
                       [[0,0, "bar", "The Empty Bottle"],
                        [0,1, "food", "Pierogi"]])

test 'My first locate_all test':
    assert my_first_example().locate_all("food") == \
        cons([0,1], None)
            
test 'My first plan_route test':
   assert my_first_example().plan_route(0, 0, "Pierogi") == \
       cons([0,0], cons([0,1], None))
        
test 'My first find_nearby test':
    assert my_first_example().find_nearby(0, 0, "food", 1) == \
        cons([0,1, "food", "Pierogi"], None)

test 'locate_all 2':
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'barber', 'Lily']])
    let result = tp.locate_all('barber')
    assert (Cons.to_vec(result)) \
      == [[5, 0], [3, 0]]
      
test 'plan_route 2':
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [2.5, 0, 'barber', 'Tony']])
    let result = tp.plan_route(0, 0, 'Tony')
    assert Cons.to_vec(result) \
      == [[0, 0], [1.5, 0], [2.5, 0]]
      
test 'plan_route 3':
    let tp = TripPlanner(
      [[0, 0, 1, 0]],
      [[0, 0, 'bank', 'Union']])
    let result = tp.plan_route(0, 0, 'Union')
    assert Cons.to_vec(result) \
      == [[0, 0]]
       
test 'plan_route 4':
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert Cons.to_vec(result) \
      == []
      
test 'plan_route 5':
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'barber', 'Judy']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert Cons.to_vec(result) \
      == []
    
test 'plan_route 6':
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'bar', 'Pasta'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'food', 'Jollibee']])
    let result = tp.plan_route(0, 0, 'Judy')
    assert Cons.to_vec(result) \
      == [[0, 0], [1.5, 0], [2.5, 0], [3, 0], [4, 0], [5, 0]]

test 'find nearby 2':
    let tp = TripPlanner(
      [[0, 0, 1.5, 0],
       [1.5, 0, 2.5, 0],
       [2.5, 0, 3, 0],
       [4, 0, 5, 0],
       [3, 0, 4, 0]],
      [[1.5, 0, 'bank', 'Union'],
       [3, 0, 'barber', 'Tony'],
       [5, 0, 'bar', 'Pasta'],
       [5, 0, 'barber', 'Judy'],
       [5, 0, 'food', 'Jollibee']])
    assert Cons.to_vec(tp.find_nearby(3, 0, "barber", 1)) == \
    [[3, 0, 'barber', 'Tony']]
    assert Cons.to_vec(tp.find_nearby(3, 0, "barber", 2)) == \
    [[5, 0, 'barber', 'Judy'], [3, 0, 'barber', 'Tony']]
    
test 'find nearby 3':
    let tp = TripPlanner(
      [[0, 0, 0, 1],
       [0, 0, 1, 0],
       [1, 0, 1, 1],
       [0, 1, 1, 1],
       [0, 1, 0, 2],
       [1, 1, 1, 2],
       [0, 2, 1, 2],
       [1, 2, 1, 3],
       [1, 3, -0.2, 3.3]],
      [[0, 0, 'bank', 'Chase'],
       [1, 0, 'food', 'Pizza'],
       [1, 0, 'bank', 'BofA'],
       [1, 3, 'bank', 'Union'],
       [1, 2, 'bar', 'Pasta'],
       [1, 2, 'barber', 'Judy'],
       [0, 2, 'food', 'Jollibee']])
    assert Cons.to_vec(tp.find_nearby(0, 1, "bank", 2)) == \
    [[1, 0, 'bank', 'BofA'], [0, 0, 'bank', 'Chase']]
    assert Cons.to_vec(tp.find_nearby(1, 2, "bank", 2)) == \
    [[1, 0, 'bank', 'BofA'], [1, 3, 'bank', 'Union']]
    assert Cons.to_vec(tp.find_nearby(1, 2, "bar", 17)) == \
    [[1, 2, 'bar', 'Pasta']]
    assert tp.find_nearby(0,1,"park",1) == None