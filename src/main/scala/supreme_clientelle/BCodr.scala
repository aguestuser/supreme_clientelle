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

//object BDecoding {
//  def popBStr(d: BDecoding) : String = d match {
//    case BStr(s) => s
//    case _ => throw new Exception ("tried to retrieve a String from a non BStr BDecoding")
//  }
//  def popBInt(d: BDecoding) : Int = d match {
//    case BInt(i) => i
//    case _ => throw new Exception ("tried to retrieve an Int from a non BInt BDecoding")
//  }
//  def popBList(d: BDecoding) : List[BDecoding] = d match {
//    case BList(l) => l
//    case _ => throw new Exception ("tried to retrieve a List from a non BList BDecoding")
//  }
//  def popBMap(d: BDecoding) : ListMap[BStr, BDecoding] = d match {
//    case BMap(m) => m
//    case _ => throw new Exception ("tried to retrieve a Map from a non BMap BDecoding")
//  }
//}

//class RetreiveStuff {
//  def getPieces(map: BMap): List[Byte] = {
//    map.is(BStr("info")) match {
//      case BMap(m2) =>
//        m2(BStr("pieces")) match {
//          case BStr(s) => BCodr.strToBytes(s)
//          case _ => throw new Exception("Whoops!")
//        }
//      case _ => throw new Exception("Whoops!")
//    }
//  }
// }

//  def getBDecoding(map: BMap, paths: List[BStr]) : BDecoding = {
//    paths match {
//      case Nil => // return a BDecoding!
//      case path :: tail => // keep looking for BDecodings!
//    }
//  }

// question: what is the idiomatic way to handle bytes in Scala?

/* answer:
* List[Byte] is the idiomatic way to handle bytes
* ie:
* someStr.getBytes => List[Byte])
* akka.util.ByteString(List[Byte]) => ByteString
* */

// question: how to destructure a bytestring?

/* answer, either:
* 'e'.toByte +: tail
* or
* ByteString('e') ++: tail
* */

// question: how to read a file to a byte array?

// question: how to sha-1 encode a byte array

/*answer:
* import java.security.MessageDigest
* val md = MessageDigest.getInstance("SHA-1")
* val byteArr = md.digest(ba)
* //or
* val byteStr = ByteString(md.digest(ba))
* */

// answer:
//import java.nio.file.{Files, Paths}
//val byteList = Files.readAllBytes(Paths.get("/Users/aguestuser/code/hackerschool/supreme_clientelle/src/test/sample_data/flagfromserver.torrent")).toList
//val d = decodeToBMap(byteList)
//val pieces = popBMap(popBMap(d)(BStr("info")))(BStr("pieces"))

// question: does scala accomodate unsigned bytes?