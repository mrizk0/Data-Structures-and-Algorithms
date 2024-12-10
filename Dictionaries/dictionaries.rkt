#lang dssl2

import sbox_hash

interface DICT[K, V]:
    def len(self) -> nat?
    def mem?(self, key: K) -> bool?
    def get(self, key: K) -> V
    def put(self, key: K, value: V) -> NoneC
    def del(self, key: K) -> NoneC
    def __print__(self, print)

struct _cons:
    let key
    let val
    let next: OrC(_cons?, NoneC)

class AssociationList[K, V] (DICT):
    let _head
    let tail
    let length

    def __init__(self):
        self._head = None
        self.tail = None
        self.length = 0
        
    def len(self):
        return self.length
        
    def mem?(self, k):
        if self.len() == 0:
            return False
        if self.len() == 1:
            if self._head.key == k:
                return True
            return False
        let spot = self._head
        while spot is not None:
            if spot.key == k:
                return True
            spot = spot.next
        return False
        
    def get(self, k):
        if self.mem?(k) == False:
            error("Key is not present.")
        for i in range(self.length):
            if self.get_nth(i).key == k:
                return self.get_nth(i).val
                
    def put(self, k, v):
        if self.mem?(k) == True:
            for i in range(self.length):
                if self.get_nth(i).key == k:
                    self.get_nth(i).val = v
        elif self.mem?(k) == False:
            let y = _cons(k,v, None)
            if self.tail == None:
                self._head = y
                self.tail = y
            self.tail.next = y
            self.tail = y
            self.length = self.length + 1
                
    def del(self,k):
        if self.mem?(k) == False:
            pass
        elif self.len() == 1:
            self._head = None
            self.tail = None
            self.length = 0
        elif self.get_head().key == k:
            self._head.key = None
            self._head.val = None
            self._head = self.get_nth(1)
            self.length = self.length - 1
        elif self.get_tail().key == k:
            self.tail.key = None
            self.tail.val = None
            self.tail.next = None
            self.get_nth(self.len()-2).next = None
            self.tail = self.get_nth(self.len()-2)
            self.length = self.length - 1
        else:
            for i in range(self.length):
                if self.get_nth(i).key == k:
                    self.get_nth(i).key = None
                    self.get_nth(i).val = None
                    self.get_nth(i-1).next = self.get_nth(i+1)
                    self.length = self.length - 1
                    break

    def __print__(self, print):
        print("#<object:AssociationList head=%p>", self._head)

    def _find_nth_node(self, n):
        let current = self._head
        while not current == None:
            if n == 0:
                return current
            n = n - 1
            current = current.next
        error('list too short')
        
    def get_nth(self, n):
        return self._find_nth_node(n)
        
    def get_head(self): return self._head
    def get_tail(self): return self.tail



class HashTable[K, V] (DICT):
    let _hash
    let _size
    let _data

    def __init__(self, nbuckets: nat?, hash: FunC[AnyC, nat?]):
        self._hash = hash
        self._size = 0
        self._data = [None; nbuckets]
        
    def __print__(self, print):
        print("#<object:HashTable  _hash=... _size=%p _data=%p>",
              self._size, self._data)
    
    def len(self):
        return self._size
        
    def mem?(self,k):
        let bucket = self._hash(k) % self._data.len()
        let loc = self._data[bucket]
        while loc != None:
            if loc.key == k:
                return True
            loc = loc.next
        return False     
                
    def get(self,k):
        let bucket = self._hash(k) % self._data.len()
        let loc = self._data[bucket]
        while loc != None:
            if loc.key == k:
                return loc.val
            loc = loc.next
        return error("Key is not present.")
        
    def put(self,k,v):
        let bucket = self._hash(k) % self._data.len()
        if self._data[bucket] == None:
            self._data[bucket] = _cons(k,v,None)
            self._size = self._size + 1
        else:
            let spot = self._data[bucket]
            while spot != None:
                if spot.key == k:
                    spot.val = v
                    return
                spot = spot.next
            let entry = _cons(k,v,None)
            entry.next = self._data[bucket]
            self._data[bucket] = entry
            self._size = self._size + 1
    
    def del(self,k):
        let bucket = self._hash(k) % self._data.len()
        let prev = None
        let spot = self._data[bucket]
        while spot != None:
            if spot.key == k:
                if prev != None:
                    prev.next = spot.next
                else:
                    self._data[bucket] = spot.next
                self._size = self._size - 1
            prev = spot
            spot = spot.next
    def get_data(self): return self._data


def first_char_hasher(s: str?) -> int?:
    if s.len() == 0:
        return 0
    else:
        return int(s[0])