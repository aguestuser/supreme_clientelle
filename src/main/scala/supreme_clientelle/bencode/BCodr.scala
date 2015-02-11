package supreme_clientelle.bencode

import supreme_clientelle.bytes.ByteReader

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator._


/**
 * Author: @aguestuser
 * Date:  2/10/15
 * License: GPLv2
 */

object BCodr {
  def decode(str: String): BDecoding = decode(str.getBytes)
  def decode(bytes: Array[Byte]): BDecoding = BDecode(bytes)
  def encode(b: BDecoding): Array[Byte] = BEncode(b)
}

object BDecode extends Parsers {

  type Elem = Byte

  def apply(in: Array[Byte]): BDecoding = apply(ByteReader(in,1))
  def apply(in: Input): BDecoding = b(in).get

  def b: Parser[BDecoding] = bInt | bStr | bList | bMap
  def bInt: Parser[BInt] = 'i'.toByte ~> digits <~'e'.toByte ^^ BInt
  def bStr: Parser[BStr] = digits flatMap { len => ':'.toByte ~> repN(len, anyByte) ^^ { x => BStr(x.toArray) } }
  def bList: Parser[BList] = 'l'.toByte ~> b <~ 'e'.toByte ^^ { bs => BList(List(bs)) }
  def bMap: Parser[BMap] = 'd'.toByte ~> rep(keyVal) <~ 'e'.toByte ^^ { kvs => BMap(listMapify(kvs))}

  private def digits: Parser[Int] = neg.? ~ rep(digit) ^^ { case(neg ~ digits) =>
    val num = digits.mkString.toInt; neg match { case None => num; case Some(_) => -num} }
  private def digit: Parser[Char] = acceptIf(('0'.toByte to '9'.toByte) contains _ )(_ => "not a digit") ^^ (_.toChar)
  private def neg: Parser[Char] = '-'.toByte ^^ { _.toChar }
  private def anyByte: Parser[Byte] = acceptIf(_ => true)(_ => "can't fail!")
  private def keyVal: Parser[(BStr,BDecoding)] = bStr ~ b ^^ { case (k ~ v) => (k,v) }
  private def listMapify(l: List[(BStr,BDecoding)]): ListMap[BStr,BDecoding] =
    ( ListMap[BStr,BDecoding]() /: l )((acc,kv) => acc + (kv._1 -> kv._2) )
}

object BEncode {
  
  def apply(b: BDecoding): Array[Byte] = encode(b)
    
  def encode(b: BDecoding): Array[Byte] = b match {
    case BInt(i) => encodeInt(i)
    case BStr(bytes) => encodeStr(bytes)
    case BList(bl) => encodeList(bl)
    case BMap(bm) => encodeMap(bm) }

  private def encodeInt(i: Int) : Array[Byte] =
  'i'.toByte +: i.toString.getBytes :+ 'e'.toByte

  private def encodeStr(bytes: Array[Byte]) : Array[Byte] =
  (bytes.size.toString.getBytes :+ ':'.toByte) ++ bytes

  private def encodeList(bl: List[BDecoding]) : Array[Byte] =
  'l'.toByte +: bl.toArray.flatMap(encode) :+ 'e'.toByte

  private def encodeMap(bm: Map[BStr,BDecoding]) : Array[Byte] =
    'd'.toByte +: {
      bm.keys.map { encode } zip bm.values.map { encode } flatMap {
        case (x, y) => x ++ y }
    }.toArray :+ 'e'.toByte

}





