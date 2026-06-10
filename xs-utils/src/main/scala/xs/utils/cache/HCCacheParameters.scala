/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  *
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

// See LICENSE.SiFive for license details.

package xs.utils.cache

import org.chipsalliance.cde.config.Parameters
import org.chipsalliance.cde.config.Field
import chisel3._
import chisel3.util.{log2Ceil, log2Up, Mux1H, Cat}
import freechips.rocketchip.diplomacy.BufferParams
import freechips.rocketchip.tilelink.{TLBufferParams, TLChannelBeatBytes, TLEdgeIn, TLEdgeOut}
import freechips.rocketchip.util.{BundleField, BundleFieldBase, BundleKeyBase, ControlKey}
import xs.utils.tl.{MemReqSource, ReqSourceKey}

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile.MaxHartIdBits
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{BundleField, BundleFieldBase, UIntToOH1}
import freechips.rocketchip.diplomacy.BundleBridgeNexusNode
import xs.utils.{ResetGen, Pipeline, FastArbiter}
import xs.utils.cache.common.{AliasKey, PreferCacheKey, PrefetchKey}
import xs.utils.cache.prefetch.{PrefetchParameters, TPmetaParameters}


case object HCCacheParamsKey extends Field[HCCacheParameters](HCCacheParameters())

case class CacheParameters
(
  name: String,
  sets: Int,
  ways: Int,
  blockGranularity: Int,
  blockBytes: Int = 64,
  aliasBitsOpt: Option[Int] = None,
  inner: Seq[CacheParameters] = Nil
) {
  val capacity = sets * ways * blockBytes
  val setBits = log2Ceil(sets)
  val offsetBits = log2Ceil(blockBytes)
  val needResolveAlias = aliasBitsOpt.nonEmpty
}

case class CacheCtrl
(
  address: BigInt,
  beatBytes: Int = 8,
  // used to generate core soft reset
  numCores: Int = 1
)

case class HCCacheParameters
(
  name: String = "L2",
  level: Int = 2,
  ways: Int = 4,
  sets: Int = 128,
  blockBytes: Int = 64,
  pageBytes: Int = 4096,
  replacement: String = "plru",
  mshrs: Int = 14,
  dirReadPorts: Int = 1,
  dirReg: Boolean = true,
  enableDebug: Boolean = false,
  enablePerf: Boolean = true,
  hartIds: Seq[Int] = Seq[Int](),
  channelBytes: TLChannelBeatBytes = TLChannelBeatBytes(32),
  prefetch: Option[PrefetchParameters] = None,
  tpmeta: Option[TPmetaParameters] = None,
  elaboratedTopDown: Boolean = true,
  clientCaches: Seq[CacheParameters] = Nil,
  inclusive: Boolean = true,
  alwaysReleaseData: Boolean = false,
  tagECC:            Option[String] = None,
  dataECC:           Option[String] = None,
  echoField: Seq[BundleFieldBase] = Nil,
  reqField: Seq[BundleFieldBase] = Nil, // master
  respKey: Seq[BundleKeyBase] = Nil,
  reqKey: Seq[BundleKeyBase] = Seq(PrefetchKey, PreferCacheKey, AliasKey, ReqSourceKey), // slave
  respField: Seq[BundleFieldBase] = Nil,
  ctrl: Option[CacheCtrl] = None,
  sramClkDivBy2: Boolean = false,
  sramDepthDiv: Int = 1,
  simulation: Boolean = false,
  innerBuf: TLBufferParams = TLBufferParams(),
  outerBuf: TLBufferParams = TLBufferParams(
    a = BufferParams.default,
    b = BufferParams.default,
    c = BufferParams.default,
    d = BufferParams.default,
    e = BufferParams.default
  ),
  FPGAPlatform: Boolean = false
) {
  require(ways > 0)
  require(sets > 0)
  require(channelBytes.d.get >= 8)
  require(dirReadPorts == 1, "now we only use 1 read port")
  if (!inclusive) {
    require(clientCaches.nonEmpty, "Non-inclusive cache need to know client cache information")
  }

  def toCacheParams: CacheParameters = CacheParameters(
    name = name,
    sets = sets,
    ways = ways,
    blockGranularity = log2Ceil(sets),
    blockBytes = blockBytes,
    inner = clientCaches
  )
}
