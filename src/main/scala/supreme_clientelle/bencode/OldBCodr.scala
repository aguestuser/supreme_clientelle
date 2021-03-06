package supreme_clientelle.bencode

import scala.collection.immutable.ListMap

/**
* Author: @aguestuser
* Date: 1/8/15
* License: GPLv2
*/

object OldBCodr {

  // public methods
  def decode(str: String) : BDecoding = decode(str.getBytes.toList)
  def decode(bytes: List[Byte]) : BDecoding = decodeOne(bytes)._1
  def encode(b: BDecoding) : List[Byte] = encodeOne(b)

  // helpers for #decode
  private def decodeOne(rem: List[Byte]) : (BDecoding, List[Byte]) = rem match {
    case 'i' :: tail => decodeInt(tail)
    case n :: tail if n.toChar.isDigit => decodeStr(rem)
    case 'l' :: tail =>  val (l, rest) = decodeList(tail); (BList(l), rest)
    case 'd' :: tail => val (l, rest) = decodeMap(tail); (BMap(l), rest)
    case bad => throw new Exception("Bencoding error starting at" + bad.take(10).map(_.toChar).mkString) }

  private def decodeInt(bytes: List[Byte]) : (BDecoding, List[Byte]) = {
    val (intBytes, _ :: tail) = bytes.span(_.toChar != 'e')
    (BInt(intBytes.map(_.toChar).mkString.toInt), tail) }

  private def decodeStr(bytes: List[Byte]) : (BDecoding, List[Byte]) = {
    val (len, _ :: tail) = bytes.span(_.toChar != ':')
    val (strBytes, newTail) = tail.splitAt(len.map(_.toChar).mkString.toInt)
    (BStr(strBytes), newTail) }

  private def decodeList(bytes: List[Byte]) : (List[BDecoding], List[Byte]) = bytes match {
    case 'e' :: tail => (List[BDecoding](), tail)
    case _ =>
      val (item, tail) = decodeOne(bytes)
      val (list, newTail) = decodeList(tail)
      (item :: list, newTail) }

  private def decodeMap(bytes: List[Byte]) : (ListMap[BStr, BDecoding], List[Byte]) = bytes match {
    case 'e' :: tail => (ListMap[BStr, BDecoding](), tail)
    case _ =>
      val (key: BStr, tail) = decodeOne(bytes)
      val (value: BDecoding, newTail) = decodeOne(tail)
      val (map, newestTail) = decodeMap(newTail)
      (ListMap(Tuple2(key, value)) ++ map, newestTail) }

  private def encodeOne(b: BDecoding) = b match {
    case BInt(i) => encodeInt(i)
    case BStr(bytes) => encodeStr(bytes)
    case BList(bl) => encodeList(bl)
    case BMap(bm) => encodeMap(bm)
  }

  // helpers for #encode

  private def encodeInt(i: Int) : List[Byte] =
    'i'.toByte :: (i.toString.getBytes.toList :+ 'e'.toByte)

  private def encodeStr(bytes: List[Byte]) : List[Byte] =
    bytes.size.toString.getBytes.toList ++ (':'.toByte :: bytes)

  private def encodeList(bl: List[BDecoding]) : List[Byte] =
    'l'.toByte :: (bl.flatMap(encodeOne) :+ 'e'.toByte)

  private def encodeMap(bm: Map[BStr,BDecoding]) : List[Byte] =
    'd'.toByte :: (
      (bm.keys.toList.map(encodeOne)
        zip bm.values.toList.map(encodeOne))
        .flatMap({ case (x,y) => x ++ y }
      ) :+ 'e'.toByte
    )
}


