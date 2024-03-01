FIRRTL version 1.2.0
circuit MockDRAM :
  module MockDRAM :
    input clock : Clock
    input reset : UInt<1>
    input io_rAddr : UInt<6> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_rEn : UInt<1> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    output io_rData_0 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    output io_rData_1 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    output io_rData_2 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    output io_rData_3 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_wAddr : UInt<6> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_wEn : UInt<1> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_wData_0 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_wData_1 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_wData_2 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]
    input io_wData_3 : UInt<8> @[\\src\\main\\scala\\hw5\\Cache.scala 26:20]

    mem dram_0 : @[\\src\\main\\scala\\hw5\\Cache.scala 35:31]
      data-type => UInt<8>
      depth => 64
      read-latency => 1
      write-latency => 1
      reader => MPORT
      writer => MPORT_1
      read-under-write => undefined
    mem dram_1 : @[\\src\\main\\scala\\hw5\\Cache.scala 35:31]
      data-type => UInt<8>
      depth => 64
      read-latency => 1
      write-latency => 1
      reader => MPORT
      writer => MPORT_1
      read-under-write => undefined
    mem dram_2 : @[\\src\\main\\scala\\hw5\\Cache.scala 35:31]
      data-type => UInt<8>
      depth => 64
      read-latency => 1
      write-latency => 1
      reader => MPORT
      writer => MPORT_1
      read-under-write => undefined
    mem dram_3 : @[\\src\\main\\scala\\hw5\\Cache.scala 35:31]
      data-type => UInt<8>
      depth => 64
      read-latency => 1
      write-latency => 1
      reader => MPORT
      writer => MPORT_1
      read-under-write => undefined
    node _GEN_0 = validif(io_rEn, io_rAddr) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 38:33]
    node _GEN_1 = validif(io_rEn, clock) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 38:33]
    node _GEN_2 = mux(io_rEn, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 35:31 38:33]
    node _GEN_3 = validif(io_rEn, dram_0.MPORT.data) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 38:26]
    node _GEN_4 = validif(io_rEn, dram_1.MPORT.data) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 38:26]
    node _GEN_5 = validif(io_rEn, dram_2.MPORT.data) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 38:26]
    node _GEN_6 = validif(io_rEn, dram_3.MPORT.data) @[\\src\\main\\scala\\hw5\\Cache.scala 37:23 38:26]
    node _GEN_7 = validif(io_wEn, io_wAddr) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:21]
    node _GEN_8 = validif(io_wEn, clock) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:21]
    node _GEN_9 = mux(io_wEn, UInt<1>("h1"), UInt<1>("h0")) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:21 35:31]
    node _GEN_10 = validif(io_wEn, UInt<1>("h1")) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:32]
    node _GEN_11 = validif(io_wEn, io_wData_0) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:32]
    node _GEN_12 = validif(io_wEn, io_wData_1) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:32]
    node _GEN_13 = validif(io_wEn, io_wData_2) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:32]
    node _GEN_14 = validif(io_wEn, io_wData_3) @[\\src\\main\\scala\\hw5\\Cache.scala 40:23 41:32]
    io_rData_0 <= _GEN_3
    io_rData_1 <= _GEN_4
    io_rData_2 <= _GEN_5
    io_rData_3 <= _GEN_6
    dram_0.MPORT.addr <= _GEN_0
    dram_1.MPORT.addr <= _GEN_0
    dram_2.MPORT.addr <= _GEN_0
    dram_3.MPORT.addr <= _GEN_0
    dram_0.MPORT.en <= _GEN_2
    dram_1.MPORT.en <= _GEN_2
    dram_2.MPORT.en <= _GEN_2
    dram_3.MPORT.en <= _GEN_2
    dram_0.MPORT.clk <= _GEN_1
    dram_1.MPORT.clk <= _GEN_1
    dram_2.MPORT.clk <= _GEN_1
    dram_3.MPORT.clk <= _GEN_1
    dram_0.MPORT_1.addr <= _GEN_7
    dram_1.MPORT_1.addr <= _GEN_7
    dram_2.MPORT_1.addr <= _GEN_7
    dram_3.MPORT_1.addr <= _GEN_7
    dram_0.MPORT_1.en <= _GEN_9
    dram_1.MPORT_1.en <= _GEN_9
    dram_2.MPORT_1.en <= _GEN_9
    dram_3.MPORT_1.en <= _GEN_9
    dram_0.MPORT_1.clk <= _GEN_8
    dram_1.MPORT_1.clk <= _GEN_8
    dram_2.MPORT_1.clk <= _GEN_8
    dram_3.MPORT_1.clk <= _GEN_8
    dram_0.MPORT_1.data <= _GEN_11
    dram_1.MPORT_1.data <= _GEN_12
    dram_2.MPORT_1.data <= _GEN_13
    dram_3.MPORT_1.data <= _GEN_14
    dram_0.MPORT_1.mask <= _GEN_10
    dram_1.MPORT_1.mask <= _GEN_10
    dram_2.MPORT_1.mask <= _GEN_10
    dram_3.MPORT_1.mask <= _GEN_10
