package FFT
import ChiselDSP._
import Chisel.{Pipe =>_,Complex => _,Mux => _, RegInit => _, RegNext => _, Counter => _, _}

class MemBankInterface[T <: DSPQnm[T]](gen : => T) extends GenDSPModule(gen) {

  val intDelay = Params.getDelays.memArbiterTop

  val A = 0
  val B = 1

  val numBanks = Params.getMem.banks

  // we, currRadNum (no delay)
  val calcCtrlFlags = (new CalcCtrlFlags).flip
  // banks, addrs (delayed)
  val calcCtrl = (new CalcCtrlO).flip
  // isMemB, we (no delay)
  val ioCtrlFlags = (new IOFlagsO).flip
  // bank, addr (delayed)
  val ioCtrl = (new nToAddrBankIO).flip
  // din, dout (delayed)
  val topIO = new FFTIO(gen)
  // x, y (delayed)
  val butterfly = (new WFTAIO(gen)).flip

  // bank associated with butterfly input index is valid
  val activeBFBanks = Pipe(Vec((0 until butterfly.x.length).map(i => calcCtrlFlags.currRadNum > DSPUInt(i))),
    Params.getDelays.calcCtrl)
  // TODO: Distribute for multiple sets of butterflies (#banks > maxRad)

  // calculation address into memory banks (if bank is active, feed in proper address; otherwise 0)
  val memCalcRAddrs = Vec((0 until numBanks).map(i => {
    calcCtrl.addrs.zipWithIndex.foldLeft(DSPUInt(0))((accum,e) => {
      val tcond = activeBFBanks(e._2) & (calcCtrl.banks(e._2) === DSPUInt(i))
      accum | (e._1 ? (tcond))
    })
  }))

  // TODO: Get rid of redundancy
  val calcRE = calcCtrlFlags.we.pipe(Params.getDelays.calcCtrl)
  // calculation read enable into memory banks
  val memCalcRE = Vec((0 until numBanks).map(i => {
    activeBFBanks.zipWithIndex.foldLeft(DSPBool(false))((accum,e) => {
    accum | (e._1 & calcRE & (calcCtrl.banks(e._2) === DSPUInt(i)))
    })
  }))

  // IO address into appropriate memory bank
  val memIORAddr = Vec((0 until numBanks).map(i => {
    ioCtrl.addr ? (ioCtrl.bank === DSPUInt(i))
  }))

  // TODO: we is misnomer, move delays back into respective controllers?
  val IORE = ioCtrlFlags.we.pipe(Params.getDelays.ioCtrl)
  // io read enable into memory banks
  val memIORE = Vec((0 until numBanks).map(i => {
    IORE & (ioCtrl.bank === DSPUInt(i))
  }))

  // Memory (A,B) for continuous flow + in-place IO
  val mems = (0 until 2).map(a => {
    val letter = if (a == A) "A" else "B"
    (0 until numBanks).map(b => {
      DSPModule(
        new Memory( Complex(gen),
                    depth = Params.getMem.lengths(b),
                    seqRead = Params.getDelays.memSeqRead,
                    outReg = Params.getDelays.memOutReg
        ),
        nameExt = "mem" + letter + "_" + b
      )
    })
  })

  // PE output straight to this module
  val rAddrTowAddrDly = Params.getDelays.memReadAtoD + Params.getDelays.pe
  // TODO: Check more optimal pipe?
  // Write enable should align up with when write data is valid
  val memCalcWE = Pipe(memCalcRE,rAddrTowAddrDly)
  val memCalcWAddrs = Pipe(memCalcRAddrs,rAddrTowAddrDly)

  // Align with rAddr, wAddr
  // Note: this is why it's important for IO + Calc controls to have the same delays
  val calcMemBR = Pipe(!ioCtrlFlags.isMemB,Params.getDelays.calcCtrl)
  // val calcMemBW = Pipe(calcMemBR,rAddrTowAddrDly)

  // TODO: Check correct calcMemBR delay (needs to finish enough before clkRatio * FFTN)

  // TODO: Make less copy-paste-y
  mems.foreach{x => x.zipWithIndex.foreach{y => {
    // A
    if (x == mems.head){
      // Handle read/write address conflict when memory is in calculation mode
      y._1.io.passThrough.get := (!calcMemBR).pipe(intDelay)
      // For IO, read/write addresses + ctrl are the same (in-place) -- read before write
      y._1.io.wAddr := Mux(calcMemBR,memIORAddr(y._2),memCalcWAddrs(y._2)).shorten(y._1.io.wAddr.getRange.max).pipe(
        intDelay
      )
      y._1.io.rAddr := Mux(calcMemBR,memIORAddr(y._2),memCalcRAddrs(y._2)).shorten(y._1.io.rAddr.getRange.max).pipe(
        intDelay
      )
      y._1.io.WE := Mux(calcMemBR,memIORE(y._2),memCalcWE(y._2)).pipe(intDelay)
    }
    // B
    else{
      // Handle read/write address conflict when memory is in calculation mode
      y._1.io.passThrough.get := calcMemBR.pipe(intDelay)
      // For IO, read/write addresses + ctrl are the same (in-place) -- read before write
      y._1.io.wAddr := Mux(calcMemBR,memCalcWAddrs(y._2),memIORAddr(y._2)).shorten(y._1.io.wAddr.getRange.max).pipe(
        intDelay
      )
      y._1.io.rAddr := Mux(calcMemBR,memCalcRAddrs(y._2),memIORAddr(y._2)).shorten(y._1.io.rAddr.getRange.max).pipe(
        intDelay
      )
      y._1.io.WE := Mux(calcMemBR,memCalcWE(y._2),memIORE(y._2)).pipe(intDelay)
    }
  }}}

  // TODO: Pipeline more? -- would need to search for intDelay dependencies or do in PE

  val memBOut = Vec(mems(B).map(x => {
    val out = x.io.dOut.cloneType
    out := x.io.dOut
    out
  }))
  val memAOut = Vec(mems(A).map(x => {
    val out = x.io.dOut.cloneType
    out := x.io.dOut
    out
  }))

  // TODO: Get rid of some redundant pipelines
  // Align with calc data out valid
  val readDelay = intDelay + Params.getDelays.memReadAtoD
  val activeBFBanksOut = Pipe(activeBFBanks,readDelay)
  val calcBanksRO = Pipe(calcCtrl.banks,readDelay)

  // Aligned with calc memory dout valid
  val calcMemBRDout = Pipe(calcMemBR,readDelay)
  val calcMemOut = Mux(calcMemBRDout,memBOut,memAOut)

  val zro = Complex(double2T(0.0),double2T(0.0))

  // Data out to butterfly (0 if unused)
  val x = Vec((0 until butterfly.x.length).map(i => {
    calcMemOut.zipWithIndex.foldLeft(zro)((accum,e) => {
      val tcond = activeBFBanksOut(i) & (calcBanksRO(i) === DSPUInt(e._2))
      accum | (e._1 ? tcond)
    })
  }))
  butterfly.x := x

  // Aligned with io memory dout valid
  val ioMemOut = Mux(calcMemBRDout,memAOut,memBOut)

  val ioBankRO = Pipe(ioCtrl.bank,readDelay)

  // Data out to output normalizer
  val out = ioMemOut.zipWithIndex.foldLeft(zro)((accum,e) => {
    val tcond = ioBankRO === DSPUInt(e._2)
    accum | (e._1 ? tcond)
  })
  topIO.dout := out

  // Match delay of data coming out of PE
  val activeBFBanksWIn = Pipe(activeBFBanksOut,Params.getDelays.pe)
  val calcBanksWI = Pipe(calcBanksRO,Params.getDelays.pe)

  // TODO: Remove redundant logic?
  // Data into memory (calculation phase)
  val memCalcDin = Vec((0 until numBanks).map(i => {
    butterfly.y.zipWithIndex.foldLeft(zro)((accum, e) => {
      val tcond = activeBFBanksWIn(e._2) & (calcBanksWI(e._2) === DSPUInt(i))
      accum | (e._1 ? (tcond))
    })
  }))

  // TODO: Should I zero out?
  // Note: din delayed to the extent that io addr/bank are delayed
  val memIODin = Pipe(Vec((0 until numBanks).map(i => {
    topIO.din ? (ioCtrl.bank === DSPUInt(i))
  })),intDelay)

  // TODO: Get rid of redundant pipe
  val calcMemBRCtrl = Pipe(calcMemBR,intDelay)
  // Pass correct input to memory
  (0 until numBanks).foreach{ i => {
    mems(B)(i).io.dIn := Mux(calcMemBRCtrl,memCalcDin(i),memIODin(i))
    mems(A)(i).io.dIn := Mux(calcMemBRCtrl,memIODin(i),memCalcDin(i))
  }}

}
