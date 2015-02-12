package supreme_clientelle.bencode

import supreme_clientelle.bytes.{ByteReader,ByteTools}
import ByteTools._

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator._


/**
 * Author: @aguestuser
 * Date:  2/10/15
 * License: GPLv2
 */

object BCodr {
  def decode(str: String): BDecoding = decode(byteVector(str))
  def decode(bytes: Vector[Byte]): BDecoding = BDecode(bytes)
  def encode(b: BDecoding): Vector[Byte] = BEncode(b)
}

object BDecode extends Parsers {

  type Elem = Byte

  def apply(in: Vector[Byte]): BDecoding = apply(ByteReader(in,1))
  def apply(in: Input): BDecoding = b(in).get

  def b: Parser[BDecoding] = bInt | bStr | bList | bMap
  def bInt: Parser[BInt] = 'i'.toByte ~> digits <~ 'e'.toByte ^^ BInt
  def bStr: Parser[BStr] = digits flatMap { len => ':'.toByte ~> repN(len, anyByte) ^^ { x => BStr(x.toVector) } }
  def bList: Parser[BList] = 'l'.toByte ~> rep(b) <~ 'e'.toByte ^^ { bs => BList(bs) }
  def bMap: Parser[BMap] = 'd'.toByte ~> rep(keyVal) <~ 'e'.toByte ^^ { kvs => BMap(listMapify(kvs))}


//  private def digits: Parser[Int] = rep(digit) ^^ { case(ds) => ds.mkString.toInt }

  private def digits: Parser[Int] = neg.? ~ rep(digit) ^^ { case(neg ~ digits) =>
    //TODO why does digits return an empty list here?
    val num = digits.mkString.toInt; neg match { case None => num; case Some(_) => 0 - num} }
//  private def digit: Parser[Char] = acceptIf(('0'.toByte to '9'.toByte) contains _ )(_ => "not a digit") ^^ (_.toChar)
  private def digit: Parser[Char] = acceptIf(Set[Byte]('0','1','2','3','4','5','6','7','8','9').contains)(_ => "not a digit") ^^ { _.toChar }
  private def neg: Parser[Char] = '-'.toByte ^^ { _.toChar }
  private def anyByte: Parser[Byte] = acceptIf(_ => true)(_ => "can't fail!")
  private def keyVal: Parser[(BStr,BDecoding)] = bStr ~ b ^^ { case (k ~ v) => (k,v) }
  private def listMapify(l: List[(BStr,BDecoding)]): ListMap[BStr,BDecoding] =
    ( ListMap[BStr,BDecoding]() /: l )((acc,kv) => acc + (kv._1 -> kv._2) )
}

object BEncode {
  
  def apply(b: BDecoding): Vector[Byte] = encode(b)
    
  def encode(b: BDecoding): Vector[Byte] = b match {
    case BInt(i) => encodeInt(i)
    case BStr(bytes) => encodeStr(bytes)
    case BList(bl) => encodeList(bl)
    case BMap(bm) => encodeMap(bm) }

  private def encodeInt(i: Int) : Vector[Byte] =
    'i'.toByte +: byteVector(i.toString) :+ 'e'.toByte

  private def encodeStr(bytes: Vector[Byte]) : Vector[Byte] =
  (byteVector(bytes.size.toString) :+ ':'.toByte) ++ bytes

  private def encodeList(bl: List[BDecoding]) : Vector[Byte] =
  'l'.toByte +: bl.toVector.flatMap(encode) :+ 'e'.toByte

  private def encodeMap(bm: Map[BStr,BDecoding]) : Vector[Byte] =
    'd'.toByte +: {
      { bm.keys zip bm.values } flatMap { case(k,v) => encode(k) ++ encode(v) }
    }.toVector :+ 'e'.toByte

}





