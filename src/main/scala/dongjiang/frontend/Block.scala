package dongjiang.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import zhujiang.chi._
import dongjiang._
import dongjiang.utils._
import dongjiang.bundle._
import xs.utils.debug._
import zhujiang.perf.HomeWrapperPerf
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.SnpOpcode._
import dongjiang.data._

class Block(implicit p: Parameters) extends DJModule {

    val io = IO(new Bundle {
        val config = new DJConfigIO()

        val chiTask_s0 = Flipped(Valid(new PackChi with HasAddr with HasQoS))
        val task_s1    = Valid(new PackChi with HasAddr with HasPackHnIdx with HasAlready with HasQoS)

        val readDir_s1 = Decoupled(new Addr with HasPackHnIdx)

        val posBlock_s1 = Input(Bool())
        val hnIdx_s1    = Input(new HnIndex())

        val retry_s1 = Output(Bool())

        val reqDB_s1 = Decoupled(new HnTxnID with HasDataVec)

        val fastResp_s1 = Decoupled(new RespFlit)
    })

    val validReg_s1    = RegInit(false.B)
    val taskReg_s1     = Reg(new PackChi with HasAddr with HasQoS)
    val sReceiptReg_s1 = RegInit(false.B)
    val sDBIDReg_s1    = RegInit(false.B)
    val shouldResp_s1  = Wire(Bool())
    val blockByDB_s1   = Wire(Bool())
    val block_s1 = Wire(new Bundle {
        val pos  = Bool()
        val dir  = Bool()
        val resp = Bool()
        def any = pos | dir | resp
    })
    dontTouch(block_s1)

    validReg_s1 := io.chiTask_s0.valid
    when(io.chiTask_s0.valid) {
        taskReg_s1 := io.chiTask_s0.bits
    }

    block_s1.pos  := io.posBlock_s1
    block_s1.dir  := taskReg_s1.chi.memAttr.cacheable & !io.readDir_s1.ready
    block_s1.resp := blockByDB_s1 | (shouldResp_s1 & !io.fastResp_s1.ready)
    io.retry_s1   := validReg_s1 & block_s1.any

    io.task_s1.valid          := validReg_s1 & !block_s1.any
    io.task_s1.bits.chi       := taskReg_s1.chi
    io.task_s1.bits.addr      := taskReg_s1.addr
    io.task_s1.bits.qos       := taskReg_s1.qos
    io.task_s1.bits.hnIdx     := io.hnIdx_s1
    io.task_s1.bits.alr.reqDB := io.reqDB_s1.fire
    io.task_s1.bits.alr.sData := false.B
    io.task_s1.bits.alr.sDBID := io.fastResp_s1.fire & io.fastResp_s1.bits.Opcode === DBIDResp

    io.readDir_s1.valid      := validReg_s1 & taskReg_s1.chi.memAttr.cacheable & !(block_s1.pos | block_s1.resp)
    io.readDir_s1.bits.addr  := taskReg_s1.addr
    io.readDir_s1.bits.hnIdx := io.hnIdx_s1

    sReceiptReg_s1 := io.chiTask_s0.bits.chi.isRead & (io.chiTask_s0.bits.chi.isEO | io.chiTask_s0.bits.chi.isRO)
    sDBIDReg_s1    := io.chiTask_s0.bits.chi.isWrite & !io.chiTask_s0.bits.chi.isCopyBackWrite
    shouldResp_s1  := sReceiptReg_s1 | (sDBIDReg_s1 & io.reqDB_s1.ready)
    blockByDB_s1   := sDBIDReg_s1 & !io.reqDB_s1.ready

    io.reqDB_s1.valid        := validReg_s1 & sDBIDReg_s1 & io.fastResp_s1.ready & !(block_s1.pos | block_s1.dir)
    io.reqDB_s1.bits.dataVec := DataVec.Full

    io.reqDB_s1.bits.hnTxnID := io.hnIdx_s1.getTxnID

    io.fastResp_s1.valid        := validReg_s1 & shouldResp_s1 & !(block_s1.pos | block_s1.dir)
    io.fastResp_s1.bits         := DontCare
    io.fastResp_s1.bits.QoS     := taskReg_s1.qos
    io.fastResp_s1.bits.SrcID   := taskReg_s1.chi.getNoC
    io.fastResp_s1.bits.TgtID   := taskReg_s1.chi.nodeId
    io.fastResp_s1.bits.TxnID   := taskReg_s1.chi.txnID
    io.fastResp_s1.bits.DBID    := io.hnIdx_s1.getTxnID
    io.fastResp_s1.bits.RespErr := RespErr.NormalOkay
    io.fastResp_s1.bits.Opcode := PriorityMux(
        Seq(
            sReceiptReg_s1 -> ReadReceipt,
            sDBIDReg_s1    -> DBIDResp
        )
    )

    private val validBlockPos  = validReg_s1 && block_s1.pos
    private val validBlockDir  = validReg_s1 && block_s1.dir
    private val validBlockResp = validReg_s1 && block_s1.resp
    HomeWrapperPerf.accumulate(
        Seq(
            ("zj_hn_block_valid", validReg_s1),
            ("zj_hn_block_pos", validBlockPos),
            ("zj_hn_block_dir", validBlockDir),
            ("zj_hn_block_resp", validBlockResp),
            ("zj_hn_block_retry", io.retry_s1),
            ("zj_hn_task_s1_valid", io.task_s1.valid),
            ("zj_hn_read_dir_fire", io.readDir_s1.fire),
            ("zj_hn_read_dir_stall", io.readDir_s1.valid && !io.readDir_s1.ready),
            ("zj_hn_req_db_fire", io.reqDB_s1.fire),
            ("zj_hn_req_db_stall", io.reqDB_s1.valid && !io.reqDB_s1.ready),
            ("zj_hn_fast_resp_fire", io.fastResp_s1.fire),
            ("zj_hn_fast_resp_stall", io.fastResp_s1.valid && !io.fastResp_s1.ready)
        )
    )

    HardwareAssertion.placePipe(1)
}
