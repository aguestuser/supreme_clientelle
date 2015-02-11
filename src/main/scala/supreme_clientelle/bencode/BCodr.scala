package supreme_clientelle.bencode

import scala.collection.immutable.ListMap
import scala.util.parsing.combinator._


/**
 * Created by aguestuser on 2/10/15.
 */

object BParser extends Parsers {
  type Elem = Byte

  def b: Parser[BDecoding] = bInt | bStr | bList | bMap
  def bInt: Parser[BInt] = 'i'.toByte ~> digits <~'e'.toByte ^^ BInt
  def bStr: Parser[BStr] = digits flatMap { len => ':'.toByte ~> repN(len, anyByte) ^^ BStr }
  def bList: Parser[BList] = 'l'.toByte ~> b <~ 'e'.toByte ^^ { bs => BList(List(bs)) }
  def bMap: Parser[BMap] = 'd'.toByte ~> rep(keyVal) <~ 'e'.toByte ^^ { kvs => BMap(listMapify(kvs))}

  private def digits: Parser[Int] = rep(digit) ^^ (_.mkString.toInt)
  private def digit: Parser[Char] = acceptIf(_.isValidInt)(_ => "not a digit") ^^ (_.toChar)
  private def anyByte: Parser[Byte] = acceptIf(_ => true)(_ => "can't fail!")
  private def keyVal: Parser[(BStr,BDecoding)] = bStr ~ b ^^ { case (k ~ v) => (k,v) }
  private def listMapify(l: List[(BStr,BDecoding)]): ListMap[BStr,BDecoding] =
    ( ListMap[BStr,BDecoding]() /: l )((acc,kv) => acc + (kv._1 -> kv._2) )
}

object BCodr {

//  def decode(bytes: List[Byte]): BDecoding = BParser.hmm...

}
