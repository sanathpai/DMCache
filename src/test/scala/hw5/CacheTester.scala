package hw5

import chisel3._
import chiseltest._
import chisel3.experimental.VecLiterals._
import org.scalatest.flatspec.AnyFlatSpec


class CacheTester extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "MockDRAM"
	it should "be able to a block & read it back" in {
		val p = CacheParams(32, 4, 4)
		test(new MockDRAM(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			val addr = 4
			val block = Vec.Lit(0.U(p.bitsPerWord.W), 1.U(p.bitsPerWord.W), 2.U(p.bitsPerWord.W), 3.U(p.bitsPerWord.W))
			dut.io.rAddr.poke(0.U)
			dut.io.rEn.poke(false.B)
			dut.io.wAddr.poke(addr.U)
			dut.io.wEn.poke(true.B)
			dut.io.wData.poke(block)
			dut.clock.step()

			dut.io.rAddr.poke(addr.U)
			dut.io.rEn.poke(true.B)
			dut.io.wEn.poke(false.B)
			dut.clock.step()

			dut.io.rData.expect(block)
		}
	}

	def performReadTest(dut: Cache, m: CacheModel, addr: Int): Unit = {
		dut.io.in.valid.poke(true.B)
		dut.io.in.bits.addr.poke(addr.U)
		dut.io.in.bits.write.poke(false.B)
		dut.io.in.ready.expect(true.B)

		dut.clock.step()
		dut.io.in.valid.poke(false.B)
		dut.io.in.ready.expect(false.B)
		if (m.isHit(addr)) {
			dut.io.hit.expect(true.B)
		} else {
			dut.io.hit.expect(false.B)
			dut.clock.step()
		}
		m.read(addr)

		dut.io.out.valid.expect(true.B)
		dut.io.in.ready.expect(false.B)
		dut.io.out.bits.expect(m.read(addr).U)

		dut.clock.step()
		dut.io.out.valid.expect(false.B)
		dut.io.in.ready.expect(true.B)
	}

	def performWriteTest(dut: Cache, m: CacheModel, addr: Int, wData: Int): Unit = {
		require(wData < (1 << dut.p.bitsPerWord))
		dut.io.in.valid.poke(true.B)
		dut.io.in.bits.addr.poke(addr.U)
		dut.io.in.bits.write.poke(true.B)
		dut.io.in.bits.wData.poke(wData.U)
		dut.io.in.ready.expect(true.B)

		dut.clock.step()
		dut.io.in.valid.poke(false.B)
		dut.io.in.ready.expect(false.B)
		if (m.isHit(addr)) {
			dut.io.hit.expect(true.B)
		} else {
			dut.io.hit.expect(false.B)
			dut.clock.step()
		}
		m.write(addr, wData)

		dut.io.out.valid.expect(true.B)
		dut.io.in.ready.expect(false.B)

		dut.clock.step()
		dut.io.out.valid.expect(false.B)
		dut.io.in.ready.expect(true.B)
	}


	behavior of "DMCache"
	it should "be able to read (miss, then hit) a block" in {
		val p = CacheParams(32, 4, 1)
		val m = CacheModel(p)()
		test(new DMCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			performReadTest(dut, m, 8)
			performReadTest(dut, m, 8)
		}
	}

	it should "be able to write miss then read hit a block" in {
		val p = CacheParams(32, 4, 1)
		val m = CacheModel(p)()
		test(new DMCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			performWriteTest(dut, m, 8, 8)
			performReadTest(dut, m, 8)
		}
	}

	it should "load in a block" in {
		val p = CacheParams(32, 4, 1)
		val m = CacheModel(p)()
		test(new DMCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			val addr = 4
			// first miss to bring block in
			performReadTest(dut, m, addr)
			// now all hits
			for (w <- 0 until p.blockSize) {
				performReadTest(dut, m, addr+w)
			}
		}
	}

	it should "be able to write to all words and then read all in cache" in {
		val p = CacheParams(32, 4, 1)
		val m = CacheModel(p)()
		test(new DMCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			for(addr <- 0 until (1 << p.addrLen)) {
				performWriteTest(dut, m, addr, addr)
			}
			for(addr <- 0 until (1 << p.addrLen)) {
				performReadTest(dut, m, addr)
			}
		}
	}

	it should "handle thrashing 0 -> 32 -> 0" in {
		val p = CacheParams(32, 4, 1)
		val m = CacheModel(p)()
		test(new DMCache(p)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
			performReadTest(dut, m, 0)							   // Read miss to block 0
			performWriteTest(dut, m, 1, 1)    // Write hit to block 0
			performWriteTest(dut, m, 32, 32)  // Write hit to block 32
			performWriteTest(dut, m, 1, 1)    // Read miss to block 0
		}
	}
}
