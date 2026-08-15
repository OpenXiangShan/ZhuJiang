package dongjiang.bundle

import chisel3._
import chisel3.util._
import dongjiang.DJBundle
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.SnpOpcode._

trait HasChiOp { this: DJBundle with HasChiChannel =>
    val opcode = UInt(ChiOpcodeBits.W)

    def reqIs(op: UInt*): Bool = {
        isReq & Cat(op.map(_ === opcode)).orR
    }

    def snpIs(op: UInt*): Bool = {
        isSnp & Cat(op.map(_ === opcode)).orR
    }

    def rspIs(op: UInt*): Bool = {
        isRsp & Cat(op.map(_ === opcode)).orR
    }

    def datIs(op: UInt*): Bool = {
        isDat & Cat(op.map(_ === opcode)).orR
    }

    def isAllocatingRead: Bool = reqIs(
        ReadClean,
        ReadNotSharedDirty,
        ReadShared,
        ReadUnique,
        ReadPreferUnique,
        MakeReadUnique,
        StashOnceShared
    )

    def isNonAllocatingRead: Bool = reqIs(
        ReadNoSnp,
        ReadNoSnpSep,
        ReadOnce,
        ReadOnceCleanInvalid,
        ReadOnceMakeInvalid
    )

    def isRead: Bool = isAllocatingRead | isNonAllocatingRead

    def isDataless: Bool = reqIs(
        CleanUnique,
        MakeUnique,
        Evict,
        StashOnceUnique,
        StashOnceSepUnique,
        StashOnceSepShared,
        CleanShared,
        CleanSharedPersist,
        CleanSharedPersistSep,
        CleanInvalid,
        CleanInvalidPoPA,
        MakeInvalid
    )

    def isWriteFull: Bool = reqIs(
        WriteNoSnpFull,
        WriteUniqueFull,
        WriteUniqueFullStash,
        WriteBackFull,
        WriteCleanFull,
        WriteEvictFull,
        WriteEvictOrEvict
    )

    def isWritePtl: Bool = reqIs(
        WriteNoSnpPtl,
        WriteUniquePtl,
        WriteUniquePtlStash,
        WriteBackPtl
    )

    def isImmediateWrite: Bool = reqIs(
        WriteNoSnpPtl,
        WriteNoSnpFull,
        WriteNoSnpDef,
        WriteUniquePtl,
        WriteUniqueFull,
        WriteUniquePtlStash,
        WriteUniqueFullStash
    )

    def isCopyBackWrite: Bool = reqIs(
        WriteBackFull,
        WriteBackPtl,
        WriteCleanFull,
        WriteEvictOrEvict,
        WriteEvictFull
    )

    def isWrite: Bool = reqIs(
        WriteNoSnpPtl,
        WriteNoSnpFull,
        WriteNoSnpZero,
        WriteNoSnpDef,
        WriteUniquePtl,
        WriteUniqueFull,
        WriteUniqueZero,
        WriteUniquePtlStash,
        WriteUniqueFullStash,
        WriteBackPtl,
        WriteBackFull,
        WriteCleanFull,
        WriteEvictFull,
        WriteEvictOrEvict
    )

    def isCombinedWrite: Bool = reqIs(
        WriteNoSnpPtlCleanInv,
        WriteNoSnpPtlCleanSh,
        WriteNoSnpPtlCleanShPerSep,
        WriteNoSnpPtlCleanInvPoPA,
        WriteNoSnpFullCleanInv,
        WriteNoSnpFullCleanSh,
        WriteNoSnpFullCleanShPerSep,
        WriteNoSnpFullCleanInvPoPA,
        WriteUniquePtlCleanSh,
        WriteUniquePtlCleanShPerSep,
        WriteUniqueFullCleanSh,
        WriteUniqueFullCleanShPerSep,
        WriteBackFullCleanInv,
        WriteBackFullCleanSh,
        WriteBackFullCleanShPerSep,
        WriteBackFullCleanInvPoPA,
        WriteCleanFullCleanSh,
        WriteCleanFullCleanShPerSep
    )

    def isAtomicStore: Bool = reqIs(
        AtomicStoreADD,
        AtomicStoreCLR,
        AtomicStoreEOR,
        AtomicStoreSET,
        AtomicStoreSMAX,
        AtomicStoreSMIN,
        AtomicStoreUMAX,
        AtomicStoreUMIN
    )

    def isAtomicLoad: Bool = reqIs(
        AtomicLoadADD,
        AtomicLoadCLR,
        AtomicLoadEOR,
        AtomicLoadSET,
        AtomicLoadSMAX,
        AtomicLoadSMIN,
        AtomicLoadUMAX,
        AtomicLoadUMIN
    )

    def isAtomicSwap: Bool = reqIs(
        AtomicSwap
    )

    def isAtomicCompare: Bool = reqIs(
        AtomicCompare
    )

    def isAtomic: Bool = isAtomicStore | isAtomicLoad | isAtomicSwap | isAtomicCompare

    def isOther: Bool = reqIs(
        DVMOp,
        PrefetchTgt,
        PCrdReturn
    )

    def getAtomicOp: UInt = opcode(3, 0)

    def reqIsLegal: Bool = reqIs(
        ReadNoSnp,
        ReadOnce,
        ReadNotSharedDirty,
        ReadUnique,
        MakeUnique,
        Evict,
        CleanShared,
        CleanInvalid,
        MakeInvalid,
        WriteNoSnpPtl,
        WriteUniquePtl,
        WriteUniqueFull,
        WriteBackFull,
        WriteCleanFull,
        WriteEvictOrEvict
    ) | isAtomicLoad | isAtomicSwap | isAtomicCompare

    def isSnpResp: Bool = rspIs(
        SnpResp,
        SnpRespFwded
    )

    def isSnpRespData: Bool = datIs(
        SnpRespData,
        SnpRespDataPtl,
        SnpRespDataFwded
    )

    def isSnpFwd: Bool = snpIs(
        SnpSharedFwd,
        SnpCleanFwd,
        SnpOnceFwd,
        SnpNotSharedDirtyFwd,
        SnpPreferUniqueFwd,
        SnpUniqueFwd
    )

    def snpIsLegal: Bool = snpIs(
        SnpOnce,
        SnpNotSharedDirty,
        SnpUnique,
        SnpMakeInvalid,
        SnpOnceFwd,
        SnpNotSharedDirtyFwd,
        SnpUniqueFwd
    )
}
