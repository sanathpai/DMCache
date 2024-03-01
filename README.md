Parameterized Cache Generator
=======================
In this assignment, you will implement a parameterized cache generator. A _cache_ is a small, fast memory used to store frequently-accessed data for quick access by a processor. To refresh your memory on cache concepts, please refer to [Finding data in a cache](https://inst.eecs.berkeley.edu/~cs61c/resources/caches.pdf) and [Cache placement policies](https://en.wikipedia.org/wiki/Cache_placement_policies). For simplicity, our cache will be blocking (handles at most one request at a time) and always write back (no dirty bits to maintain). Our cache will be either direct mapped or set associative (not fully associative). The first part of the assignment requires you to implement the functionality of the cache in Scala. This model will then be used to verify your Chisel code from the second part of this assignment. 

In this assignment, in some places, we provide more code than normal to reduce unnecessary work and focus on the learning goals: inheritance and memories (SyncReadMem). That being said, in some blanks, you may need to write 50+ lines of code.


## Problem 1 - Functional Model - CacheModel

First, create a functional model of a cache in Scala using the inheritance concepts we learned in lecture. The abstract base class `CacheModel` is provided for you. It defines an interface for all of our functional cache models as well as providing helper functions (e.g. extract fields from address). You will need to fill in the bodies of the the two concrete children: `DMCacheModel` (direct mapped) and `SACacheModel` (set associative). Although a set-associative cache is generalizes caches (direct mapped and fully associative are special cases of it), we will still implement the direct mapped cache separately. You can think of the direct mapped cache as both a familiar concept to experiment with inheritance with and also a simpler design to close the loop early before moving on to set associative.

Inside CacheModel, you will notice that the `read` and `write` operations are already implemented, but they use the method `getReferenceToBlock` you need to implement in the child classes. To act as a backing memory (e.g. DRAM), we use a larger ArrayBuffer `externalMem`. When copying to and from it, to ensure a _deep copy_, use `.clone`. Within the model, `CacheBlockModel` represents a cache block. Under the hood, it is a type alias for an ArrayBuffer. In the child classes, you will also need to declare you own structures to hold the cache contents (tags, data, valid), and we recommend using ArrayBuffers.

You will have to implement:
* `getReferenceToBlock` for both caches. This method returns the cache block (an ArrayBuffer) by _reference_, so any modifications to the block will be reflected in the cache. If the requested block is not present in the cache, this method should process the cache miss first (bring that block into the cache), and then return a reference to that result.
* `isHit` for both caches. This method does not change the internal contents or state of the cache, but simply returns if the requested address is present in the cache. This will be helpful for testing the hit/miss functionality of our Chisel module.
* `wayToReplace` only for SACacheModel. With an associative cache, there is a choice of where to put in a new block (replacement policy). For ease of testing, we will use a round-robin strategy starting from way 0 in our model.

For the set-associative cache `SACacheModel`, you will implement it with your direct-mapped cache `DMCacheModel`. Each _way_ of your set associative cache is a direct-mapped cache (that is scaled down so the overall capacity is the same). We provide some of this declaration to ease setting this up. By reusing the direct-mapped cache, you can focus on what differentiates the set-associative cache, namely having multiple ways and having a replacement policy.



## Problem 2 - Chisel Generator 

After familiarizing yourself with the cache internals by implementing the model, you are now ready to build the `Cache` generator in Chisel. It is parameterized by capacity, block size, and address length. We use a case class `CacheParams` to encapsulate these. Our Chisel module will only implement a direct mapped cache (`DMCache`) since the complexity of the set-associative cache would make this assignment too long. Like the functional model, the abstract base class `Cache` defines the interfaces, provides useful helper operations, and is provided to you. You will need to fill in `DMCache`.

You can assume the cache will only be asked to perform one request at a time. Additionally, the request will remain on the input until it is completed, so you will not need to capture it internally. The hardware interface to `Cache` uses `Valid` and `Decoupled` to indicate availability and progress. The IO `in` is used to initiate a request when it is valid and the cache is ready (i.e. _fire_ on Decoupled). Inside the `in` Bundle, there is the memory address (`addr`) and `write` which if true stores `wData` to `addr` and if false performs a read from `addr`. The IO `out` is used by the cache to give the result of a request. For a write (which returns no data), the cache still uses the valid signal in `out` to indicate it is done. The cache also provides the output `hit` to capture the behavior for testing. The `hit` signal is updated the cycle after the request is sent and accepted.

Inside your cache, you should use `SyncReadMem`s to hold the data and tags. To hold the valid bits, we recommend using a `Reg` of `Vec` so you can initialize them all to 0 for "start-up." 

To simplify things, we provide the external memory for the cache `MockDRAM`. It even includes a read port and a write port to avoid any structural hazards. When your cache has a miss, it will need to fetch the block from this memory. Likewise, any time a block is evicted from the cache, it will need to be written back to this memory. With the two ports, you can perform these two operations simultaneously without any issue. The external memory has a fixed latency of only a single cycle for all operations which is unrealistic, but easier to design for.

To guide your cache implementation, we recommend the following 3-state FSM:
* _Ready_ - When the cache is not currently serving a request. When a request comes in, the cache should start reading its tag & data memories and move onto the `Lookup` state.
* _Lookup_ - Based on the results of the tag lookups (and consulting the valid bits), the cache will be able to determine if there was a hit. If there was a hit, it either returns the data (read) or updates its data array (write). If it is a miss, the cache sends off a request to the external memory for the missing block and moves to the `Fetch` state.
* _Fetch_ - When the missing block returns from the external memory, it is simultaneously stored in the cache and returned to the processor (if it is a read).


### Tips
* We provide type aliases to make it easier to declare cache blocks (`CacheBlockModel` and `CacheBlock`)
* Although you can use `when` blocks to control when a SyncReadMem is accessed, you may find it more predictable and understandable to use its `read` and `write` methods directly on the SyncReadMems, which also have an enable signal (Bool `en`).
* When debugging, you may find it helpful to reduce some of the parameters in the provided test cases.
