package dongjiang.frontend.decode

import dongjiang._
import dongjiang.frontend._
import dongjiang.frontend.decode.Decode.DecodeType
import dongjiang.frontend.decode.Inst._
import dongjiang.frontend.decode.Code._
import dongjiang.frontend.decode.DecodeCHI._
import dongjiang.bundle._
import dongjiang.bundle.ChiChannel._
import zhujiang.chi.ReqOpcode._
import zhujiang.chi.RspOpcode._
import zhujiang.chi.DatOpcode._
import zhujiang.chi.SnpOpcode._
import zhujiang.chi._
import chisel3._
import chisel3.util._

object Write_LAN {

    def writeNoSnpPtl_noEWA_EO: DecodeType = (
        fromLAN | toLAN | reqIs(WriteNoSnpPtl) | isEO,
        Seq(
            (sfMiss | llcIs(I)) -> (returnDBID, Seq(NCBWrData -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp))))
        )
    )

    def writeNoSnpPtl_noEWA_OWO: DecodeType = (
        fromLAN | toLAN | reqIs(WriteNoSnpPtl) | isOWO,
        Seq(
            (sfMiss | llcIs(I)) -> (returnDBID, Seq(NCBWrData -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp))))
        )
    )

    def writeNoSnpPtl_ewa_OWO: DecodeType = (
        fromLAN | toLAN | reqIs(WriteNoSnpPtl) | ewa | isOWO,
        Seq(
            (sfMiss | llcIs(I)) -> (returnDBID, Seq(NCBWrData -> second(tdop("send") | write(WriteNoSnpPtl), cmtRsp(Comp))))
        )
    )

    def writeNoSnpPtlTable: Seq[DecodeType] = Seq(writeNoSnpPtl_noEWA_EO, writeNoSnpPtl_noEWA_OWO, writeNoSnpPtl_ewa_OWO)

    def writeUniquePtl_noEWA: DecodeType = (
        fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO,
        Seq(
            (sfMiss | llcIs(I))  -> (returnDBID, Seq(NCBWrData -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp)))),
            (sfMiss | llcIs(SC)) -> (returnDBID, Seq(NCBWrData -> second(tdop("read", "merge", "send", "fullSize") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriLLC(I)))),
            (sfMiss | llcIs(UC)) -> (returnDBID, Seq(NCBWrData -> second(tdop("read", "merge", "send", "fullSize") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriLLC(I)))),
            (sfMiss | llcIs(UD)) -> (returnDBID, Seq(NCBWrData -> second(tdop("read", "merge", "send", "fullSize") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriLLC(I)))),
            (srcMiss | othHit | llcIs(I)) -> (returnDBID | snpOth(SnpUnique) | retToSrc | needDB, Seq(
                (NCBWrData | datIs(SnpRespData) | respIs(I_PD)) -> second(tdop("send", "fullSize") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriSNP(false)),
                (NCBWrData | datIs(SnpRespData) | respIs(I))    -> second(tdop("send", "fullSize") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriSNP(false)),
                (NCBWrData | rspIs(SnpResp) | respIs(I))        -> second(tdop("send") | write(WriteNoSnpPtl), waitSecDone | cmtRsp(Comp) | wriSNP(false))
            ))
        )
    )
    def writeUniquePtl_noEWA_allocate: DecodeType = (fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | allocate, writeUniquePtl_noEWA._2)
    def writeUniquePtl_noEWA_fullSize: DecodeType = (fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | isFullSize, writeUniquePtl_noEWA._2)
    def writeUniquePtl_noEWA_allocate_fullSize: DecodeType = (fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | allocate | isFullSize, writeUniquePtl_noEWA._2)

    def writeUniquePtl_ewa_noAllocate: DecodeType = (
        fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | ewa,
        Seq(
            (sfMiss | llcIs(I))  -> (returnDBID, Seq(NCBWrData -> second(tdop("send") | write(WriteNoSnpPtl), cmtRsp(Comp)))),
            (sfMiss | llcIs(SC)) -> first(returnDBID, NCBWrData, cdop("read", "save", "fullSize") | cmtRsp(Comp) | wriLLC(UD)),
            (sfMiss | llcIs(UC)) -> first(returnDBID, NCBWrData, cdop("read", "save", "fullSize") | cmtRsp(Comp) | wriLLC(UD)),
            (sfMiss | llcIs(UD)) -> first(returnDBID, NCBWrData, cdop("read", "save", "fullSize") | cmtRsp(Comp) | wriLLC(UD)),
            (srcMiss | othHit | llcIs(I)) -> (returnDBID | snpOth(SnpUnique) | retToSrc | needDB, Seq(
                (NCBWrData | datIs(SnpRespData) | respIs(I_PD)) -> second(cdop("save", "fullSize") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UD)),
                (NCBWrData | datIs(SnpRespData) | respIs(I))    -> second(cdop("save", "fullSize") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UD)),
                (NCBWrData | rspIs(SnpResp) | respIs(I))        -> second(tdop("send") | write(WriteNoSnpPtl), cmtRsp(Comp) | wriSNP(false))
            ))
        )
    )
    def writeUniquePtl_ewa_noAllocate_fullSize: DecodeType = (fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | ewa | isFullSize, writeUniquePtl_ewa_noAllocate._2)

    def writeUniquePtl_ewa_allocate: DecodeType = (fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | ewa | allocate, writeUniquePtl_ewa_noAllocate._2)
    def writeUniquePtl_ewa_allocate_fullSize: DecodeType = (
        fromLAN | toLAN | reqIs(WriteUniquePtl) | isOWO | ewa | allocate | isFullSize,
        Seq(
            (sfMiss | llcIs(I))  -> (returnDBID, Seq(NCBWrData -> second(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), waitSecDone | cdop("save") | cmtRsp(Comp) | wriLLC(UD)))),
            (sfMiss | llcIs(SC)) -> first(returnDBID, NCBWrData, cdop("read", "save") | cmtRsp(Comp) | wriLLC(UD)),
            (sfMiss | llcIs(UC)) -> first(returnDBID, NCBWrData, cdop("read", "save") | cmtRsp(Comp) | wriLLC(UD)),
            (sfMiss | llcIs(UD)) -> first(returnDBID, NCBWrData, cdop("read", "save") | cmtRsp(Comp) | wriLLC(UD)),
            (srcMiss | othHit | llcIs(I)) -> (returnDBID | snpOth(SnpUnique) | retToSrc | needDB, Seq(
                (NCBWrData | datIs(SnpRespData) | respIs(I_PD)) -> second(cdop("save") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UD)),
                (NCBWrData | datIs(SnpRespData) | respIs(I))    -> second(cdop("save") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UD)),
                (NCBWrData | rspIs(SnpResp) | respIs(I))        -> second(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), waitSecDone | cdop("save") | cmtRsp(Comp) | wriSNP(false) | wriLLC(UD))
            ))
        )
    )

    def writeUniquePtlTable: Seq[DecodeType] = Seq(
        writeUniquePtl_noEWA,
        writeUniquePtl_noEWA_allocate,
        writeUniquePtl_noEWA_fullSize,
        writeUniquePtl_noEWA_allocate_fullSize,
        writeUniquePtl_ewa_noAllocate,
        writeUniquePtl_ewa_noAllocate_fullSize,
        writeUniquePtl_ewa_allocate,
        writeUniquePtl_ewa_allocate_fullSize
    )

    def writeBackFull_noAlloc: DecodeType = (
        fromLAN | toLAN | reqIs(WriteBackFull) | ewa | noOrder | isFullSize,
        Seq(
            (sfMiss | llcIs(I))           -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(SC))          -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(UC))          -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(UD))          -> first(returnDBID, CBRespIs(I), noCmt),
            (srcMiss | othHit | llcIs(I)) -> first(returnDBID, CBRespIs(I), noCmt),
            (srcHit | othMiss | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(UD_PD) -> second(tdop("send") | write(WriteNoSnpFull), wriSRC(false)),
                CBRespIs(UC)    -> second(tdop("send") | write(WriteNoSnpFull), wriSRC(false)),
                CBRespIs(SC)    -> second(tdop("send") | write(WriteNoSnpFull), wriSRC(false)),
                CBRespIs(I)     -> second(wriSRC(false))
            )),
            (srcHit | othHit | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(SC) -> second(wriSRC(false)),
                CBRespIs(I)  -> second(wriSRC(false))
            ))
        )
    )

    def writeBackFull_alloc: DecodeType = (
        fromLAN | toLAN | reqIs(WriteBackFull) | allocate | ewa | noOrder | isFullSize,
        Seq(
            (sfMiss | llcIs(I))           -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(SC))          -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(UC))          -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(UD))          -> first(returnDBID, CBRespIs(I), noCmt),
            (srcMiss | othHit | llcIs(I)) -> first(returnDBID, CBRespIs(I), noCmt),
            (srcHit | othMiss | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(UD_PD) -> second(cdop("save") | wriSRC(false) | wriLLC(UD)),
                CBRespIs(UC)    -> second(cdop("save") | wriSRC(false) | wriLLC(UC)),
                CBRespIs(SC)    -> second(cdop("save") | wriSRC(false) | wriLLC(UC)),
                CBRespIs(I)     -> second(wriSRC(false))
            )),
            (srcHit | othHit | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(SC) -> second(wriSRC(false)),
                CBRespIs(I)  -> second(wriSRC(false))
            ))
        )
    )

    def writeEvictOrEvict: DecodeType = (
        fromLAN | toLAN | reqIs(WriteEvictOrEvict) | expCompAck | allocate | ewa | noOrder | isFullSize,
        Seq(
            (sfMiss | llcIs(I))           -> first(cmtRsp(Comp)),
            (sfMiss | llcIs(SC))          -> first(cmtRsp(Comp)),
            (sfMiss | llcIs(UC))          -> first(cmtRsp(Comp)),
            (sfMiss | llcIs(UD))          -> first(cmtRsp(Comp)),
            (srcMiss | othHit | llcIs(I)) -> first(cmtRsp(Comp)),
            (srcHit | othMiss | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(UD_PD) -> second(cdop("save") | wriSRC(false) | wriLLC(UD)),
                CBRespIs(UC)    -> second(cdop("save") | wriSRC(false) | wriLLC(UC)),
                CBRespIs(SC)    -> second(cdop("save") | wriSRC(false) | wriLLC(UC)),
                CBRespIs(I)     -> second(wriSRC(false))
            )),
            (srcHit | othHit | llcIs(I)) -> first(cmtRsp(Comp) | wriSRC(false))
        )
    )

    def writeCleanFull: DecodeType = (
        fromLAN | toLAN | reqIs(WriteCleanFull) | ewa | noOrder | isFullSize,
        Seq(
            (sfMiss | llcIs(I))           -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(SC))          -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(UC))          -> first(returnDBID, CBRespIs(I), noCmt),
            (sfMiss | llcIs(UD))          -> first(returnDBID, CBRespIs(I), noCmt),
            (srcMiss | othHit | llcIs(I)) -> first(returnDBID, CBRespIs(I), noCmt),
            (srcHit | othMiss | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(UD_PD) -> second(tdop("send") | write(WriteNoSnpFull), noCmt),
                CBRespIs(UC)    -> second(noCmt),
                CBRespIs(SC)    -> second(noCmt),
                CBRespIs(I)     -> second(noCmt),
                // CHI WriteCleanFull permits dirty shared data (SD_PD).
                // Transfer its dirty responsibility to memory while preserving
                // the retained source copy and any other sharers.
                CBRespIs(SD_PD) -> second(tdop("send") | write(WriteNoSnpFull), noCmt)
            )),
            (srcHit | othHit | llcIs(I)) -> (returnDBID, Seq(
                CBRespIs(SC) -> second(noCmt),
                CBRespIs(I)  -> second(noCmt),
                CBRespIs(SD_PD) -> second(tdop("send") | write(WriteNoSnpFull), noCmt)
            ))
        )
    )

    def table: Seq[DecodeType] = writeNoSnpPtlTable ++ writeUniquePtlTable ++ Seq(writeEvictOrEvict, writeBackFull_noAlloc, writeBackFull_alloc, writeCleanFull)
}
