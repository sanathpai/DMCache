file:///C:/Users/Dell/OneDrive/Desktop/hw5-main/src/main/scala/hw5/Cache.scala
### java.lang.IndexOutOfBoundsException: 0

occurred in the presentation compiler.

presentation compiler configuration:
Scala version: 3.3.1
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.1\scala3-library_3-3.3.1.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.10\scala-library-2.13.10.jar [exists ]
Options:



action parameters:
offset: 3746
uri: file:///C:/Users/Dell/OneDrive/Desktop/hw5-main/src/main/scala/hw5/Cache.scala
text:
```scala
package hw5

import chisel3._
import chisel3.util._

case class CacheParams(capacity: Int, blockSize: Int, associativity: Int, addrLen: Int = 8, bitsPerWord: Int = 8) {
	require((1 << addrLen) >= capacity)
	require(capacity > blockSize)
	require(isPow2(capacity) && isPow2(blockSize) && isPow2(associativity) && isPow2(bitsPerWord))
	// inputs capacity & blockSize are in units of words

	val numExtMemBlocks = (1 << addrLen) / blockSize
	val memBlockAddrBits = log2Ceil(numExtMemBlocks)

	val numSets = capacity / blockSize / associativity
	val numOffsetBits = log2Ceil(blockSize)
	val numIndexBits = log2Ceil(numSets)
	val numTagBits = addrLen - (numOffsetBits + numIndexBits)
}


class MockDRAM(p: CacheParams) extends Module {
	def CacheBlock(): Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))

	// addresses in terms of blocks
	val io = IO(new Bundle {
		val rAddr = Input(UInt(p.memBlockAddrBits.W))
		val rEn = Input(Bool())
		val rData = Output(CacheBlock())
		val wAddr = Input(UInt(p.memBlockAddrBits.W))
		val wEn = Input(Bool())
		val wData = Input(CacheBlock())
	})
	// Fixed memory latency of 1 cycle
	val dram = SyncReadMem(p.numExtMemBlocks, CacheBlock())
	io.rData := DontCare
	when (io.rEn) {
		io.rData := dram(io.rAddr)
	}
	when (io.wEn) {
		dram(io.wAddr) := io.wData
	}
}


class Cache(val p: CacheParams) extends Module {
	val io = IO(new Bundle {
		val in = Flipped(Decoupled(new Bundle {
			val addr = UInt(p.addrLen.W)
			val write = Bool()
			val wData = UInt(p.bitsPerWord.W)
		}))
		val hit = Output(Bool())                  // helpful for testing
		val out = Valid(UInt(p.bitsPerWord.W))		// sets valid to true to indicate completion (even for writes)
	})

	// extract fields from address
	val tag    = io.in.bits.addr(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
	val index  = io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
	val offset = io.in.bits.addr(p.numOffsetBits - 1, 0)

	// essentially making a type alias to make it easy to declare
	def CacheBlock(): Vec[UInt] = Vec(p.blockSize, UInt(p.bitsPerWord.W))

	// backing memory
	val extMem = Module(new MockDRAM(p))
}

class DMCache(p: CacheParams) extends Cache(p) {
  require(p.associativity == 1, "DMCache only supports direct mapping (associativity = 1)")

  val dataMem = SyncReadMem(p.numSets, Vec(p.blockSize, UInt(p.bitsPerWord.W)))
  val tagMem = SyncReadMem(p.numSets, UInt(p.numTagBits.W))
  val validBits = RegInit(VecInit(Seq.fill(p.numSets)(false.B)))

  val sReady :: sLookup :: sFetch :: Nil = Enum(3)
  val state = RegInit(sReady)

  // Assuming all addresses are block-aligned for simplicity
  val addrIndex = io.in.bits.addr(p.addrLen-1, p.numOffsetBits)
  val blockOffset = io.in.bits.addr(p.numOffsetBits-1, 0)
  val inputTag = io.in.bits.addr(p.addrLen-1, p.addrLen - p.numTagBits)

  when(io.in.fire()) {
    state := sLookup
    // Initiate tag and data read
  }

  // Interface to external DRAM (MockDRAM)
  extMem.io.rEn := false.B
  extMem.io.wEn := false.B

  switch(state) {
    is(sReady) {
      io.in.ready := true.B
      when(io.in.valid) {
        state := sLookup
        // Read operations for tag and data are initiated here
      }
    }

    is(sLookup) {
      val readTag = tagMem.read(addrIndex, true.B) // Enable read
      val readData = dataMem.read(addrIndex, true.B)
      val isValid = validBits(addrIndex)

      when(isValid && (readTag === inputTag)) {
        // Cache hit
        io.hit := true.B
        when(io.in.bits.write) {
          // Write-through policy: Update cache and external memory
          val updatedData = readData.updated(blockOffset.asUInt()@@, io.in.bits.wData)
          dataMem.write(addrIndex, updatedData)
          extMem.io.wData := updatedData // Ensure this is correctly sized
          extMem.io.wAddr := addrIndex
          extMem.io.wEn := true.B
        }.otherwise {
          // Read hit, drive output
          io.out.bits := readData(blockOffset)
          io.out.valid := true.B
        }
        state := sReady
      }.otherwise {
        // Cache miss, initiate fetch
        extMem.io.rEn := true.B
        extMem.io.rAddr := addrIndex
        state := sFetch
      }
    }

    is(sFetch) {
      // Handle response from external memory
      when(extMem.io.rEn) {
        val fetchedData = extMem.io.rData // Assuming direct connection
        dataMem.write(addrIndex, fetchedData)
        tagMem.write(addrIndex, inputTag)
        validBits(addrIndex) := true.B
        // Optionally, directly satisfy the cache miss by outputting fetched data
        state := sReady
      }
    }
  }

  // Ready to accept new input when in sReady state
  io.in.ready := (state === sReady)
}

```



#### Error stacktrace:

```
scala.collection.LinearSeqOps.apply(LinearSeq.scala:131)
	scala.collection.LinearSeqOps.apply$(LinearSeq.scala:128)
	scala.collection.immutable.List.apply(List.scala:79)
	dotty.tools.dotc.util.Signatures$.countParams(Signatures.scala:501)
	dotty.tools.dotc.util.Signatures$.applyCallInfo(Signatures.scala:186)
	dotty.tools.dotc.util.Signatures$.computeSignatureHelp(Signatures.scala:94)
	dotty.tools.dotc.util.Signatures$.signatureHelp(Signatures.scala:63)
	scala.meta.internal.pc.MetalsSignatures$.signatures(MetalsSignatures.scala:17)
	scala.meta.internal.pc.SignatureHelpProvider$.signatureHelp(SignatureHelpProvider.scala:51)
	scala.meta.internal.pc.ScalaPresentationCompiler.signatureHelp$$anonfun$1(ScalaPresentationCompiler.scala:398)
```
#### Short summary: 

java.lang.IndexOutOfBoundsException: 0