package xs.utils.sram

import chisel3._
import chisel3.util._
import chisel3.experimental.hierarchy.{instantiable, public, Definition, Instance}
import scala.collection.mutable

class SramMbistIO extends Bundle {
  val dft_ram_bypass = Input(Bool())
  val dft_ram_bp_clken = Input(Bool())
}

class SramBroadcastBundle extends Bundle {
  val ram_hold = Input(Bool())
  val ram_bypass = Input(Bool())
  val ram_bp_clken = Input(Bool())
  val ram_aux_clk = Input(Bool())
  val ram_aux_ckbp = Input(Bool())
  val ram_mcp_hold = Input(Bool())
  val cgen = Input(Bool())
}

class GenericSramPowerCtl extends Bundle {
  val light_sleep = Input(Bool())
  val deep_sleep = Input(Bool())
  val shut_down = Input(Bool())
}

class SramCtrlBundle extends Bundle {
  val cfg = Input(UInt(32.W))
}


@instantiable
class SramArray(
  depth: Int,
  width: Int,
  maskSegments: Int,
  hasMbist: Boolean,
  sramName: Option[String] = None,
  powerCtl: Boolean,
  singlePort: Boolean,
  delayRead:Boolean
)
  extends RawModule {
  require(width % maskSegments == 0)
  @public val mbist = if(hasMbist) Some(IO(new SramMbistIO)) else None
  @public val pwctl = if(powerCtl) Some(IO(new GenericSramPowerCtl)) else None
  @public val ctrl = IO(new SramCtrlBundle)
  dontTouch(ctrl)
  mbist.foreach(dontTouch(_))
  pwctl.foreach(dontTouch(_))

  @public val RW0 = if(singlePort) Some(IO(new SpRamRwIO(width, maskSegments, depth))) else None
  @public val R0 = if(!singlePort) Some(IO(new DpRamRIO(width, depth))) else None
  @public val W0 = if(!singlePort) Some(IO(new DpRamWIO(width, maskSegments, depth))) else None

  // Use SyncReadMem instead of SramInstGen blackbox so that gsim can
  // generate C++ from the FIRRTL representation.
  // Follow the same pattern as utility/src/main/scala/utility/sram/SramProto.scala
  // to ensure compatible FIRRTL output (smem without ", new").
  if (singlePort) {
    val rw = RW0.get
    if (maskSegments > 1) {
      val dataType = Vec(maskSegments, UInt((width / maskSegments).W))
      val array = SyncReadMem(depth, dataType)
      rw.rdata := array.readWrite(rw.addr, rw.wdata.asTypeOf(dataType), rw.wmask.get.asBools, rw.en, rw.wmode, rw.clk).asUInt
    } else {
      val array = SyncReadMem(depth, UInt(width.W))
      rw.rdata := array.readWrite(rw.addr, rw.wdata, rw.en, rw.wmode, rw.clk)
    }
  } else {
    val r = R0.get
    val w = W0.get
    if (maskSegments > 1) {
      val dataType = Vec(maskSegments, UInt((width / maskSegments).W))
      val array = SyncReadMem(depth, dataType)
      when(w.en) {
        array.write(w.addr, w.data.asTypeOf(dataType), w.mask.get.asBools, w.clk)
      }
      r.data := array.read(r.addr, r.en, r.clk).asUInt
    } else {
      val array = SyncReadMem(depth, UInt(width.W))
      when(w.en) {
        array.write(w.addr, w.data, w.clk)
      }
      r.data := array.read(r.addr, r.en, r.clk)
    }
  }

  override def desiredName: String = sramName.getOrElse(super.desiredName)
}

object SramProto {
  private val defMap = mutable.Map[String, Definition[SramArray]]()

  def init(sram: Instance[SramArray], singlePort: Boolean, clock: Clock, writeClock: Option[Clock]): Unit = {
    if(singlePort) {
      dontTouch(sram.RW0.get)
      sram.RW0.get := DontCare
      sram.RW0.get.clk := clock
      sram.RW0.get.en := false.B
    } else {
      dontTouch(sram.R0.get)
      dontTouch(sram.W0.get)
      sram.R0.get := DontCare
      sram.R0.get.clk := clock
      sram.R0.get.en := false.B
      sram.W0.get := DontCare
      sram.W0.get.clk := writeClock.getOrElse(clock)
      sram.W0.get.en := false.B
    }
  }

  def read(sram: Instance[SramArray], singlePort: Boolean, addr: UInt, enable: Bool): UInt = {
    if(singlePort) {
      sram.RW0.get.addr := addr
      sram.RW0.get.en := enable
      sram.RW0.get.wmode := false.B
      sram.RW0.get.rdata
    } else {
      sram.R0.get.addr := addr
      sram.R0.get.en := enable
      sram.R0.get.data
    }
  }

  def write(sram: Instance[SramArray], singlePort: Boolean, addr: UInt, data: UInt, mask: UInt): Unit = {
    if(singlePort) {
      sram.RW0.get.addr := addr
      sram.RW0.get.en := true.B
      sram.RW0.get.wmode := true.B
      sram.RW0.get.wmask.foreach(_ := mask)
      sram.RW0.get.wdata := data
    } else {
      sram.W0.get.addr := addr
      sram.W0.get.en := true.B
      sram.W0.get.mask.foreach(_ := mask)
      sram.W0.get.data := data
    }
  }

  def apply(
    clock: Clock,
    singlePort: Boolean,
    depth: Int,
    width: Int,
    maskSegments: Int = 1,
    setup: Int,
    hold: Int,
    latency: Int,
    writeClock: Option[Clock] = None,
    hasMbist: Boolean,
    suffix: String = "",
    powerCtl: Boolean
  ): (Instance[SramArray], String) = {
    val mcpStr = s"s${setup}h${hold}l${latency}"
    val pwStr = if(powerCtl) "p" else ""
    val mbist = if(hasMbist) "b" else ""
    val numPort = if(singlePort) 1 else 2
    val maskWidth = width / maskSegments
    val sramName = Some(s"sram_array_${numPort}p${depth}x${width}m$maskWidth$mcpStr$pwStr$mbist$suffix")
    if(!defMap.contains(sramName.get)) {
      defMap(sramName.get) = Definition(new SramArray(
        depth = depth,
        width = width,
        maskSegments = maskSegments,
        hasMbist = hasMbist,
        sramName = sramName,
        powerCtl = powerCtl,
        singlePort = singlePort,
        delayRead = latency > 1
        ))
    }
    val array = Instance(defMap(sramName.get))
    SramProto.init(array, singlePort, clock, writeClock)
    (array, sramName.get)
  }
}
