#lang dssl2
#lang dssl2

interface PRIORITY_QUEUE[X]:
    # Returns the number of elements in the priority queue.
    def len(self) -> nat?
    # Returns the smallest element; error if empty.
    def find_min(self) -> X
    # Removes the smallest element; error if empty.
    def remove_min(self) -> NoneC
    # Inserts an element; error if full.
    def insert(self, element: X) -> NoneC

# Class implementing the PRIORITY_QUEUE ADT as a binary heap.
class BinHeap[X] (PRIORITY_QUEUE):
    let _data: VecC[OrC(X, NoneC)]
    let _size: nat?
    let _lt?:  FunC[X, X, bool?]

    # Constructs a new binary heap with the given capacity and
    # less-than function for type X.
    def __init__(self, capacity, lt?):
        self._size = 0
        self._lt? = lt?
        self._data = [None; capacity]
        
    def len(self):
        return self._size
    
    def find_min(self):
        if self._size == 0:
            error("Priority queue is empty.")
        return self._data[0]
        
    def remove_min(self):
        if self._size == 0:
            error("Priority queue is empty.")
        self._data[0] = self._data[self._size - 1]
        self._size = self._size - 1
        self._data[self._size] = None
        self.percolate_down(0)
        
    def insert(self, element):
        if self._size == self._data.len():
            error("Priority queue is full")
        self._data[self._size] = element
        self._size = self._size + 1
        self.bubble_up(self._size - 1)
        
    def bubble_up(self,i):
        while i > 0:
            if self._lt?(self._data[i], self._data[self.parent(i)]) == True:
                self.swap(i, self.parent(i))
                i = self.parent(i)
            else:
                break
    def percolate_down(self,i):
        while i < self._size:
            let spot = i
            if self.leftc(i) < self._size and self._lt?(self._data[self.leftc(i)], self._data[spot]) == True:
                spot = self.leftc(i)
            if self.rightc(i) < self._size and self._lt?(self._data[self.rightc(i)], self._data[spot]) == True:
                spot = self.rightc(i)
            if spot != i:
                self.swap(i,spot)
                i = spot
            else:
                break
    def swap(self,x,y):
        let a = self._data[x]
        let b = self._data[y]
        self._data[x] = b
        self._data[y] = a
    def parent(self,i):
        return ((i-1)/2).floor()
    def leftc(self,i):
        return 2*i+1
    def rightc(self,i):
        return 2*i+2
    def get_data(self):
        return self._data



# Sorts a vector of Xs, given a less-than function for Xs.
#
# This function performs a heap sort by inserting all of the
# elements of v into a fresh heap, then removing them in
# order and placing them back in v.
def heap_sort[X](v: VecC[X], lt?: FunC[X, X, bool?]) -> NoneC:
    let heap = BinHeap(v.len(), lt?)
    for i in v:
        heap.insert(i)
    for i in range(v.len()):
        v[i] = heap.find_min()
        heap.remove_min()

struct person:
    let name: str?
    let birth_month: nat?
    let birth_day: nat?

def earliest_birthday() -> str?:
    def func(x,y):
        if x.birth_month < y.birth_month:
            return True
        elif x.birth_month == y.birth_month:
            if x.birth_day < y.birth_day:
                return True
            else:
                return False
        return False
    let b = [None;5]
    b[0] = person("Jean-Roch",11,12)
    b[1] = person("Gabrielle",8,25)
    b[2] = person("Sylvie",8,7)
    b[3] = person("Pascal",12,20)
    b[4] = person("Julie",8,12)
    heap_sort(b, func)
    return b[0].name
#   ^ YOUR CODE GOES HERE
    
test 'earliest_birthday':
    assert earliest_birthday() == "Sylvie"