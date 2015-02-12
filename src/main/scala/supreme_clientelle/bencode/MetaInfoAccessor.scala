package supreme_clientelle.bencode

import supreme_clientelle.bencode.BCodr._
import supreme_clientelle.bencode.BDecoding._
import supreme_clientelle.bytes.ByteTools._

/**
 * Author: @aguestuser
 * Date: 1/9/15
 * License: GPLv2
 */

object MetaInfoAccessor {

  def getAnnounceUrl(b: BDecoding) : String = lookupAndStringify(b, List(Bmk("announce")))
  def getLength(b: BDecoding) : Int = lookupAndIntify(b, List(Bmk("info"),Bmk("length")))
  def getLengthAsStr(b: BDecoding) : String = getLength(b).toString
  def getInfoHash(b: BDecoding) : String = (hash _ andThen escape)(encode(lookup(b, List(Bmk("info"))).get))
  def getPeerIdHash(b: BDecoding) : String = "-AG-0000-" + escape(hash(byteVector("Austin Spencer Guest")).take(11))

}
