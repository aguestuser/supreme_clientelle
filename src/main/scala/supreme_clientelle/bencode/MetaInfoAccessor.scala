package supreme_clientelle.bencode

import BDecoding._
import BCodr._


/**
 * Created by aguestuser on 1/20/15.
 */
object MetaInfoAccessor extends ByteTools {

  def getAnnounceUrl(b: BDecoding) : String = lookupAndStringify(b, List(Bmk("announce")))
  def getLength(b: BDecoding) : Int = lookupAndIntify(b, List(Bmk("info"),Bmk("length")))
  def getLengthAsStr(b: BDecoding) : String = getLength(b).toString
  def getInfoHash(b: BDecoding) : String = {
    val infoMap = lookup(b, List(Bmk("info"))).get
    (hash _ andThen escape)(encode(infoMap).toArray)
  }
  def getPeerIdHash(b: BDecoding) : String = {// TODO randomize this!
    "-AG-0000-" + escape(hash("Austin Spencer Guest".getBytes).take(11))
  }

  def hash(bytes: Array[Byte]) : Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-1").digest(bytes)

  def escape(bytes: Array[Byte]) : String = {
    val allowed = Set('0','1','2','3','4','5','6','7','8','9',
      'A','B','C','D','E','F','G','H','I','J','K','L','M',
      'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
      'a','b','c','d','e','f','g','h','i','j','k','l','m',
      'n','o','p','q','r','s','t','u','v','w','x','y','z',
      '.','-','_','~')
      .map(_.toByte)
    bytes
      .map(charifyOrHexify(allowed))
      .map(doubleDigitize)
      .mkString
  }

  private def charifyOrHexify(allowed: Set[Byte])(b: Byte) : String =
    if (allowed.contains(b)) b.toChar.toString else hexify(b)

  private def hexify(b: Byte) : String =
    "%" + b.toHexString.replace("ffffff","").toUpperCase

  private def doubleDigitize(hex: String) : String =
    """(%)([A-Z|0-9])(%|$)""".r.replaceAllIn(hex, m => (m group 1) + "0" + (m group 2) )


}
