package dongjiang.frontend.decode

import dongjiang._
import dongjiang.frontend._
import dongjiang.frontend.decode.Decode.DecodeType
import dongjiang.frontend.decode.Inst._
import dongjiang.frontend.decode.Code.{wriSRC, _}
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

object Read_LAN_DCT_DMT {

    def readNoSnp_noExpCompAck_EO: DecodeType = (
        fromLAN | toLAN | reqIs(ReadNoSnp) | isEO,
        Seq(
            (sfMiss | llcIs(I)) -> first(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), cdop("send") | cmtDat(CompData) | respIs(I))
        )
    )

    def readNoSnp_expCompAck_EO: DecodeType = (
        fromLAN | toLAN | reqIs(ReadNoSnp) | expCompAck | isEO,
        Seq(
            (sfMiss | llcIs(I)) -> first(read(ReadNoSnp) | doDMT, noCmt)
        )
    )
    def readNoSnp_expCompAck_EO_ewa: DecodeType = (fromLAN | toLAN | reqIs(ReadNoSnp) | expCompAck | isEO | ewa, readNoSnp_expCompAck_EO._2)

    def readNoSnpTable: Seq[DecodeType] = Seq(readNoSnp_noExpCompAck_EO, readNoSnp_expCompAck_EO, readNoSnp_expCompAck_EO_ewa)

    def readOnce_noAllocate: DecodeType = (
        fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO,
        Seq(
            (sfMiss | llcIs(I))  -> first(read(ReadNoSnp) | doDMT, noCmt),
            (sfMiss | llcIs(SC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(I)),
            (sfMiss | llcIs(UC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(I)),
            (sfMiss | llcIs(UD)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpOne(SnpOnceFwd) | needDB, Seq(
                (rspIs(SnpRespFwded) | respIs(UC) | fwdIs(I))        -> second(noCmt),
                (rspIs(SnpRespFwded) | respIs(SC) | fwdIs(I))        -> second(noCmt),
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(I))         -> second(wriSNP(false)),
                (rspIs(SnpRespFwded) | respIs(UD) | fwdIs(I))        -> second(noCmt),
                (datIs(SnpRespDataFwded) | respIs(SC_PD) | fwdIs(I)) -> second(tdop("send", "fullSize") | write(WriteNoSnpFull), noCmt),
                (datIs(SnpRespDataFwded) | respIs(I_PD) | fwdIs(I))  -> second(cdop("save") | wriSNP(false) | wriLLC(UD)),
                (rspIs(SnpResp) | respIs(SC))                        -> second(read(ReadNoSnp) | doDMT, noCmt),
                (rspIs(SnpResp) | respIs(I))                         -> second(read(ReadNoSnp) | doDMT, wriSNP(false))
            ))
        )
    )
    def readOnce_noAllocate_ewa: DecodeType = (fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | ewa, readOnce_noAllocate._2)
    def readOnce_noAllocate_fullSize: DecodeType = (fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | isFullSize, readOnce_noAllocate._2)
    def readOnce_noAllocate_ewa_fullSize: DecodeType = (fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | ewa | isFullSize, readOnce_noAllocate._2)

    def readOnce_allocate: DecodeType = (fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | allocate, readOnce_noAllocate._2)
    def readOnce_allocate_ewa: DecodeType = (fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | allocate | ewa, readOnce_noAllocate._2)
    def readOnce_allocate_fullSize: DecodeType = (
        fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | allocate | isFullSize,
        Seq(
            (sfMiss | llcIs(I))  -> first(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), cdop("send", "save") | cmtDat(CompData) | respIs(I) | wriLLC(UC)),
            (sfMiss | llcIs(SC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(I)),
            (sfMiss | llcIs(UC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(I)),
            (sfMiss | llcIs(UD)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpOne(SnpOnceFwd) | needDB, Seq(
                (rspIs(SnpRespFwded) | respIs(UC) | fwdIs(I))        -> second(noCmt),
                (rspIs(SnpRespFwded) | respIs(SC) | fwdIs(I))        -> second(noCmt),
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(I))         -> second(wriSNP(false)),
                (rspIs(SnpRespFwded) | respIs(UD) | fwdIs(I))        -> second(noCmt),
                (datIs(SnpRespDataFwded) | respIs(SC_PD) | fwdIs(I)) -> second(tdop("send", "fullSize") | write(WriteNoSnpFull), noCmt),
                (datIs(SnpRespDataFwded) | respIs(I_PD) | fwdIs(I))  -> second(cdop("save") | wriSNP(false) | wriLLC(UD)),
                (rspIs(SnpResp) | respIs(SC))                        -> second(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), waitSecDone | cdop("send") | cmtDat(CompData) | respIs(I)),
                (rspIs(SnpResp) | respIs(I))                         -> second(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), waitSecDone | cdop("send") | cmtDat(CompData) | respIs(I) | wriSNP(false))
            ))
        )
    )
    def readOnce_allocate_ewa_fullSize: DecodeType = (fromLAN | toLAN | reqIs(ReadOnce) | expCompAck | isEO | allocate | ewa | isFullSize, readOnce_allocate_fullSize._2)

    def readOnceTable: Seq[DecodeType] = Seq(
        readOnce_noAllocate,
        readOnce_noAllocate_ewa,
        readOnce_noAllocate_fullSize,
        readOnce_noAllocate_ewa_fullSize,
        readOnce_allocate,
        readOnce_allocate_ewa,
        readOnce_allocate_fullSize,
        readOnce_allocate_ewa_fullSize
    )

    def readNotSharedDirty: DecodeType = (
        fromLAN | toLAN | reqIs(ReadNotSharedDirty) | expCompAck | noOrder | allocate | ewa | isFullSize,
        Seq(
            (sfMiss | llcIs(I))  -> first(read(ReadNoSnp) | doDMT, wriSRC(true)),
            (sfMiss | llcIs(SC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (sfMiss | llcIs(UC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (sfMiss | llcIs(UD)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(UD_PD) | wriSRC(true) | wriLLC(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpOne(SnpNotSharedDirtyFwd) | needDB, Seq(
                (rspIs(SnpRespFwded) | respIs(SC) | fwdIs(SC))        -> second(wriSRC(true)),
                (datIs(SnpRespDataFwded) | respIs(SC) | fwdIs(SC))    -> second(wriSRC(true)),
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(SC))         -> second(wriSRC(true) | wriSNP(false)),
                (datIs(SnpRespDataFwded) | respIs(I) | fwdIs(SC))     -> second(wriSRC(true) | wriSNP(false)),
                (datIs(SnpRespDataFwded) | respIs(SC_PD) | fwdIs(SC)) -> second(tdop("send") | write(WriteNoSnpFull), waitSecDone | wriSRC(true)),
                (datIs(SnpRespDataFwded) | respIs(I_PD) | fwdIs(SC))  -> second(tdop("send") | write(WriteNoSnpFull), waitSecDone | wriSRC(true) | wriSNP(false)),
                (rspIs(SnpResp) | respIs(I))                          -> second(read(ReadNoSnp), datIs(CompData) | respIs(UC), waitSecDone | cdop("send") | cmtDat(CompData) | resp(SC) | wriSRC(true) | wriSNP(false))
            )),
            (srcHit | othMiss | llcIs(I)) -> first(read(ReadNoSnp) | doDMT, noCmt),
            (srcHit | othHit | llcIs(I)) -> (snpOne(SnpNotSharedDirtyFwd) | needDB, Seq(
                (rspIs(SnpRespFwded) | respIs(SC) | fwdIs(SC))     -> second(noCmt),
                (datIs(SnpRespDataFwded) | respIs(SC) | fwdIs(SC)) -> second(noCmt),
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(SC))      -> second(wriSNP(false)),
                (datIs(SnpRespDataFwded) | respIs(I) | fwdIs(SC))  -> second(wriSNP(false)),
                (rspIs(SnpResp) | respIs(I))                       -> second(read(ReadNoSnp), datIs(CompData) | respIs(UC), waitSecDone | cdop("send") | cmtDat(CompData) | resp(SC) | wriSNP(false))
            ))
        )
    )

    def readUnique: DecodeType = (
        fromLAN | toLAN | reqIs(ReadUnique) | expCompAck | noOrder | allocate | ewa | isFullSize,
        Seq(
            (sfMiss | llcIs(I))  -> first(read(ReadNoSnp) | doDMT, wriSRC(true)),
            (sfMiss | llcIs(SC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (sfMiss | llcIs(UC)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(UC) | wriSRC(true) | wriLLC(I)),
            (sfMiss | llcIs(UD)) -> first(cdop("read", "send") | cmtDat(CompData) | resp(UD_PD) | wriSRC(true) | wriLLC(I)),
            (srcMiss | othHit | llcIs(I)) -> (snpOth(SnpUniqueFwd) | needDB, Seq(
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(UC))    -> second(wriSRC(true) | wriSNP(false)),
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(UD_PD)) -> second(wriSRC(true) | wriSNP(false)),
                (datIs(SnpRespData) | respIs(I_PD))              -> second(cdop("send") | cmtDat(CompData) | resp(UD_PD) | wriSRC(true) | wriSNP(false)),
                (rspIs(SnpResp) | respIs(I))                     -> second(read(ReadNoSnp) | doDMT, wriSRC(true) | wriSNP(false))
            )),
            (srcHit | othMiss | llcIs(I)) -> first(read(ReadNoSnp) | doDMT, noCmt),
            (srcHit | othHit | llcIs(I)) -> (snpOth(SnpUniqueFwd) | needDB, Seq(
                (rspIs(SnpRespFwded) | respIs(I) | fwdIs(UC)) -> second(wriSRC(true) | wriSNP(false)),
                (rspIs(SnpResp) | respIs(I))                  -> second(read(ReadNoSnp) | doDMT, wriSRC(true) | wriSNP(false))
            ))
        )
    )

    def stashOnceShared: DecodeType = (
        fromLAN | toLAN | reqIs(StashOnceShared) | noOrder | allocate | ewa | isFullSize,
        Seq(
            (sfMiss | llcIs(I)) -> first(read(ReadNoSnp) | needDB, datIs(CompData) | respIs(UC), cdop("save") | cmtRsp(Comp) | resp(I) | wriLLC(UC)),
            (sfMiss | llcIs(SC)) -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(UC)) -> first(cmtRsp(Comp) | resp(I)),
            (sfMiss | llcIs(UD)) -> first(cmtRsp(Comp) | resp(I)),
            (srcMiss | othHit | llcIs(I)) -> first(cmtRsp(Comp) | resp(I)),
            (srcHit | othMiss | llcIs(I)) -> first(cmtRsp(Comp) | resp(I)),
            (srcHit | othHit | llcIs(I)) -> first(cmtRsp(Comp) | resp(I))
        )
    )

    def table: Seq[DecodeType] = readNoSnpTable ++ readOnceTable ++ Seq(readNotSharedDirty, readUnique, stashOnceShared)
}
