package supreme_clientelle

/**
 * Created by aguestuser on 1/8/15.
 */

abstract class BDecoding
case class BStr(is: String) extends BDecoding
case class BInt(is: Int) extends BDecoding
case class BList(is: List[BDecoding]) extends BDecoding
case class BMap(is: Map[String, BDecoding]) extends BDecoding

object BCodr {

  def decode(str: String) : List[BDecoding] = decode(strToBytes(str))
  def decode(bytes: List[Byte]) : List[BDecoding] = decodeMany(bytes)._1

  def decodeMany(rem: List[Byte]) : (List[BDecoding], List[Byte]) = rem match {
    case Nil => (List[BDecoding](), rem)
    case _ =>
      val (res, tail) = decodeOne(rem)
      val (list, newTail) = decodeMany(tail)
      (res :: list, newTail)
  }

  def decodeOne(rem: List[Byte]) : (BDecoding, List[Byte]) = rem match {
    case 'i' :: tail => decodeInt(tail)
    case n :: tail if n.toChar.isDigit => decodeStr(rem)
    case 'l' :: tail =>  val (l, rest) = decodeList(tail); (BList(l), rest)
    case 'd' :: tail => val (l, rest) = decodeMap(tail); (BMap(l), rest)
    case _ => throw new IllegalArgumentException(
      "Looks like you didn't encode something according to spec... Don't worry!" +
        "Here's the spec: https://wiki.theory.org/BitTorrentSpecification#Lists"
    )
  }

  def decodeInt(bytes: List[Byte]) : (BDecoding, List[Byte]) = {
    val (intBytes, _ :: tail) = bytes.span(_.toChar != 'e')
    (BInt(bytesToStr(intBytes).toInt), tail)
  }

  def decodeStr(bytes: List[Byte]) : (BDecoding, List[Byte]) = {
    val (len, _ :: tail) = bytes.span(_.toChar != ':')
    val (strBytes, newTail) = tail.splitAt(bytesToStr(len).toInt)
    (BStr(bytesToStr(strBytes)), newTail)
  }

  def decodeList(bytes: List[Byte]) : (List[BDecoding], List[Byte]) = bytes match {
    case 'e' :: tail => (List[BDecoding](), tail)
    case _ =>
      val (item, tail) = decodeOne(bytes)
      val (list, newTail) = decodeList(tail)
      (item :: list, newTail)
  }

  def decodeMap(bytes: List[Byte]) : (Map[String, BDecoding], List[Byte]) = bytes match {
    case 'e' :: tail => (Map[String, BDecoding](), tail)
    case _ =>
      val (key: BStr, tail) = decodeOne(bytes)
      val (value: BDecoding, newTail) = decodeOne(tail)
      val (map, newestTail) = decodeMap(newTail)
      (map + Tuple2(key.is, value), newestTail)
//      val k = (key.is, value)
//      (map + k, newestTail)
  }

  // utility
  def strToBytes(str: String) : List[Byte] = str.getBytes.toList
  def bytesToStr(bytes: List[Byte]) : String = bytes.map(_.toChar).mkString
}

// to do:
  // * add a parser combinator
  //    * that does decodeOne several times
  //    * would look something like this: def many(p : Parser[A]): Parser[List[A]]
  //    * a type alias for Parser[A] might help