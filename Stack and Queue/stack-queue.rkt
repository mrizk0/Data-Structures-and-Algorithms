#lang dssl2

import ring_buffer

interface STACK[T]:
    def push(self, element: T) -> NoneC
    def pop(self) -> T
    def empty?(self) -> bool?
    
# interface QUEUE[T]:
#     def enqueue(self, element: T) -> NoneC
#     def dequeue(self) -> T
#     def empty?(self) -> bool?

# Linked-list node struct (implementation detail):
struct _cons:
    let data
    let next: OrC(_cons?, NoneC)

###
### ListStack
###

class ListStack[T] (STACK):

    # Any fields you may need can go here.
    let head
    let tail
    let length

    # Constructs an empty ListStack.
    def __init__ (self):
        self.head = None
        self.tail = None
        self.length = 0
    
    def push(self,element):
        self.head = _cons(element, self.head)
        if self.tail == None:
            self.tail = self.head
        self.length = self.length + 1
        return None
        
    def pop(self):
        if self.empty?() == True:
            error('List is empty')
        let y = self.head.data
        self.head = self.head.next
        self.length = self.length - 1
        return y
        
    def empty?(self):
        if self.get_length() == 0:
            return True
        elif self.get_length() > 0:
            return False

    def get_length(self): return self.length
    def get_head(self): return self.head.data
    def get_tail(self): return self.tail.data

### ListQueue

class ListQueue[T] (QUEUE):

    # Any fields you may need can go here.
    let head
    let tail
    let length

    # Constructs an empty ListQueue.
    def __init__ (self):
        self.head = None
        self.tail = None
        self.length = 0
        
    def enqueue(self, element):
        let y = _cons(element, None)
        if self.tail == None:
            self.head = y
            self.tail = y
        self.tail.next = y
        self.tail = y
        self.length = self.length + 1
        
    def dequeue(self):
        if self.empty?() == True:
            error('List is empty')
        let y = self.head
        self.head = y.next
        if self.head == None:
            self.tail = None
        self.length = self.length - 1
        return y.data
        
    def empty?(self):
        if self.get_length() == 0:
            return True
        elif self.get_length() > 0:
            return False

    def get_length(self): return self.length
    def get_head(self): return self.head.data
    def get_tail(self): return self.tail.data