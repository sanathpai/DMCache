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
	require(p.associativity == 1)



	val stateReady = RegInit(true.B)
	val stateLookup = RegInit(false.B)
	val stateFetch = RegInit(false.B)



	val dMemory = SyncReadMem(p.numSets, Vec(p.blockSize, UInt(p.bitsPerWord.W)))
	val tMemory = SyncReadMem(p.numSets, UInt(p.numTagBits.W))
	val validBits = RegInit(VecInit(Seq.fill(p.numSets)(false.B)))



	val outputValid = RegInit(false.B)
	val hit = RegInit(false.B)
	val outputBits = RegInit(0.U(p.bitsPerWord.W))


	val extrEn = RegInit(false.B)
	val extrAddr = RegInit(0.U(p.memBlockAddrBits.W))
	extMem.io.wAddr := 0.U
	val extwEn = RegInit(true.B)
	extMem.io.wData := VecInit(Seq.fill(p.blockSize)(0.U(p.bitsPerWord.W)))


	val addressPointer = io.in.bits.addr(p.numOffsetBits + p.numIndexBits - 1, p.numOffsetBits)
	val addressTag = io.in.bits.addr(p.addrLen - 1, p.numOffsetBits + p.numIndexBits)
	val blockExtra = io.in.bits.addr(p.numOffsetBits - 1, 0)

	when(stateReady) {
			io.in.ready := true.B
			when(io.in.fire) {
				val readTag = tMemory.read(addressPointer)
				val isValid = validBits(addressPointer)
				val isHit = (readTag === addressTag) && isValid
				hit := isHit
				when(isHit){
					outputValid := true.B
				}
				stateReady := false.B
				stateLookup := true.B
				stateFetch := false.B
			}
	}.elsewhen(stateLookup) {
		val readTag = tMemory.read(addressPointer)
		val isValid = validBits(addressPointer)
		val isHit = (readTag === addressTag) && isValid
		hit := isHit
		when(hit) {
			val readData = dMemory.read(addressPointer)
			when(io.in.bits.write) {
				val newData = Wire(Vec(p.blockSize, UInt(p.bitsPerWord.W)))
				for (i <- 0 until p.blockSize) {
					newData(i) := readData(i)
					when(blockExtra === i.U && extwEn) {
						newData(i) := io.in.bits.wData
					}
				}

				when(extwEn) {
					dMemory.write(addressPointer, newData)
					tMemory.write(addressPointer, addressTag)
					validBits(addressPointer) := true.B
				}
				outputBits := newData(blockExtra)
				outputValid := false.B
				stateReady := true.B
				stateLookup := false.B
				stateFetch := false.B
			}
				.otherwise {
				outputValid := false.B
				outputBits := readData(blockExtra)
				stateReady := true.B
				stateLookup := false.B
				stateFetch := false.B
			}
		}.otherwise {
			hit := false.B
			extrAddr := addressPointer
			extrEn := true.B
			outputValid := true.B
			stateReady := false.B
			stateLookup := false.B
			stateFetch := true.B
		}
	}.elsewhen(stateFetch) {
		hit := false.B
			val fetchedData = extMem.io.rData // Assuming extMem.io.rData is the fetched data
			when(extrEn) {
				dMemory.write(addressPointer, fetchedData)
				tMemory.write(addressPointer, addressTag)
				validBits(addressPointer) := true.B
				outputValid := false.B
				stateReady := true.B
				stateLookup := false.B
				stateFetch := false.B

				val readData = dMemory.read(addressPointer)
				when(io.in.bits.write) {
					val newData = Wire(Vec(p.blockSize, UInt(p.bitsPerWord.W)))
					for (i <- 0 until p.blockSize) {
						newData(i) := readData(i)
						when( extwEn && blockExtra === i.U) {
							newData(i) := io.in.bits.wData
						}
					}

					when(extwEn) {
						dMemory.write(addressPointer, newData)
						tMemory.write(addressPointer, addressTag)
						validBits(addressPointer) := true.B
					}
					outputBits := newData(blockExtra)
				}
			}
		}
	io.in.ready := stateReady
	io.out.valid := outputValid
	io.hit := hit
	io.out.bits := outputBits
	extMem.io.rEn := extrEn
	extMem.io.wEn := extwEn
	extMem.io.rAddr := extrAddr
}

