package supreme_clientelle

import scala.collection.immutable.ListMap

/**
 * Created by aguestuser on 1/8/15.
 */

object BCodr extends Util {

  def decode(str: String) : BDecoding = decode(str.getBytes.toList)
  def decode(bytes: List[Byte]) : BDecoding = decodeOne(bytes)._1

  private def decodeOne(rem: List[Byte]) : (BDecoding, List[Byte]) = rem match {
    case 'i' :: tail => decodeInt(tail)
    case n :: tail if n.toChar.isDigit => decodeStr(rem)
    case 'l' :: tail =>  val (l, rest) = decodeList(tail); (BList(l), rest)
    case 'd' :: tail => val (l, rest) = decodeMap(tail); (BMap(l), rest)
    case bad => throw new IllegalArgumentException("Bencoding error starting at" + bytesToStr(bad.take(10))) }

  private def decodeInt(bytes: List[Byte]) : (BDecoding, List[Byte]) = {
    val (intBytes, _ :: tail) = bytes.span(_.toChar != 'e')
    (BInt(bytesToStr(intBytes).toInt), tail) }

  private def decodeStr(bytes: List[Byte]) : (BDecoding, List[Byte]) = {
    val (len, _ :: tail) = bytes.span(_.toChar != ':')
    val (strBytes, newTail) = tail.splitAt(bytesToStr(len).toInt)
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

}

// question: how to sha-1 encode a byte array?

/*answer:
* import java.security.MessageDigest
* val ba = someByteList.toArray
* val md = MessageDigest.getInstance("SHA-1")
* val bhash:List[Byte] = md.digest(ba).toList
* */

// question: how to read a file into a byte array?

/* answer:
* import java.nio.file.{Files, Paths}
* val byteList = Files.readAllBytes(Paths.get("path/to/file")).toList
* */

