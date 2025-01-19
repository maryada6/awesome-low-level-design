package lfucache

import (
	"sync"
)

// Node represents a node in the doubly linked list
// Each node has a key, value, frequency, and pointers to other nodes
type Node[K comparable, V any] struct {
	key       K
	value     V
	frequency int
	prev      *Node[K, V]
	next      *Node[K, V]
}

// FrequencyList represents a doubly linked list to manage nodes with the same frequency
type FrequencyList[K comparable, V any] struct {
	head *Node[K, V]
	tail *Node[K, V]
}

// LFUCache represents a thread-safe LFU cache implementation
type LFUCache[K comparable, V any] struct {
	capacity int
	cache    map[K]*Node[K, V]
	freqMap  map[int]*FrequencyList[K, V]
	minFreq  int
	mu       sync.RWMutex
}

// NewLFUCache creates a new LFU cache with the specified capacity
func NewLFUCache[K comparable, V any](capacity int) *LFUCache[K, V] {
	return &LFUCache[K, V]{
		capacity: capacity,
		cache:    make(map[K]*Node[K, V]),
		freqMap:  make(map[int]*FrequencyList[K, V]),
	}
}

// Get retrieves a value from the cache
func (c *LFUCache[K, V]) Get(key K) (V, bool) {
	c.mu.Lock()
	defer c.mu.Unlock()

	node, exists := c.cache[key]
	if !exists {
		var zero V
		return zero, false
	}

	c.updateNodeFrequency(node)
	return node.value, true
}

// Put adds or updates a value in the cache
func (c *LFUCache[K, V]) Put(key K, value V) {
	c.mu.Lock()
	defer c.mu.Unlock()

	if c.capacity == 0 {
		return
	}

	if node, exists := c.cache[key]; exists {
		node.value = value
		c.updateNodeFrequency(node)
		return
	}

	if len(c.cache) >= c.capacity {
		c.evict()
	}

	newNode := &Node[K, V]{
		key:       key,
		value:     value,
		frequency: 1,
	}

	c.cache[key] = newNode
	if _, exists := c.freqMap[1]; !exists {
		c.freqMap[1] = &FrequencyList[K, V]{
			head: &Node[K, V]{},
			tail: &Node[K, V]{},
		}
		c.freqMap[1].head.next = c.freqMap[1].tail
		c.freqMap[1].tail.prev = c.freqMap[1].head
	}
	c.addToFrequencyList(newNode, c.freqMap[1])
	c.minFreq = 1
}

// updateNodeFrequency updates the frequency of a node and moves it to the correct frequency list
func (c *LFUCache[K, V]) updateNodeFrequency(node *Node[K, V]) {
	oldFreq := node.frequency
	node.frequency++
	c.removeFromFrequencyList(node, c.freqMap[oldFreq])

	if oldFreq == c.minFreq && c.freqMap[oldFreq].head.next == c.freqMap[oldFreq].tail {
		delete(c.freqMap, oldFreq)
		c.minFreq++
	}

	if _, exists := c.freqMap[node.frequency]; !exists {
		c.freqMap[node.frequency] = &FrequencyList[K, V]{
			head: &Node[K, V]{},
			tail: &Node[K, V]{},
		}
		c.freqMap[node.frequency].head.next = c.freqMap[node.frequency].tail
		c.freqMap[node.frequency].tail.prev = c.freqMap[node.frequency].head
	}
	c.addToFrequencyList(node, c.freqMap[node.frequency])
}

// evict removes the least frequently used node from the cache
func (c *LFUCache[K, V]) evict() {
	if list, exists := c.freqMap[c.minFreq]; exists {
		leastUsedNode := list.head.next
		c.removeFromFrequencyList(leastUsedNode, list)
		delete(c.cache, leastUsedNode.key)
		if list.head.next == list.tail {
			delete(c.freqMap, c.minFreq)
		}
	}
}

// addToFrequencyList adds a node to a frequency list
func (c *LFUCache[K, V]) addToFrequencyList(node *Node[K, V], list *FrequencyList[K, V]) {
	node.next = list.head.next
	node.prev = list.head
	list.head.next.prev = node
	list.head.next = node
}

// removeFromFrequencyList removes a node from a frequency list
func (c *LFUCache[K, V]) removeFromFrequencyList(node *Node[K, V], list *FrequencyList[K, V]) {
	node.prev.next = node.next
	node.next.prev = node.prev
}

// Size returns the current number of items in the cache
func (c *LFUCache[K, V]) Size() int {
	c.mu.RLock()
	defer c.mu.RUnlock()
	return len(c.cache)
}

// Clear removes all items from the cache
func (c *LFUCache[K, V]) Clear() {
	c.mu.Lock()
	defer c.mu.Unlock()

	c.cache = make(map[K]*Node[K, V])
	c.freqMap = make(map[int]*FrequencyList[K, V])
	c.minFreq = 0
}
