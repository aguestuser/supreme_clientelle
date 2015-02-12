package supreme_clientelle.bencode

import supreme_clientelle.bytes.ByteTools._

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}


/**
 * Author: @aguestuser
 * Date: 1/14/15
 * License: GPLv2
 */

sealed trait BDecoding
case class BStr(is: Vector[Byte]) extends BDecoding { override def toString = "BStr(BStrify(" + is.map(_.toChar).mkString +"))" }
case class BInt(is: Int) extends BDecoding
case class BList(is: List[BDecoding]) extends BDecoding
case class BMap(is: ListMap[BStr, BDecoding]) extends BDecoding

sealed trait BKey
case class Bmk(is: String) extends BKey
case class Blk(is: Int) extends BKey

object BDecoding extends BKey {

  def BStrify(str: String): BStr = BStr(byteVector(str))

  def lookupAndStringify(b: BDecoding, keys: List[BKey]) : String = stringify(lookup(b,keys).get).get
  def lookupAndIntify(b: BDecoding, keys: List[BKey]) : Int = intify(lookup(b,keys).get).get

  def stringify(b: BDecoding): Try[String] = {
    val typeErr = curryTypeErr("strify", "BStr")_
    b match {
      case BStr(s) => Success(s.map(_.toChar).mkString)
      case BInt(i) => Failure(typeErr("BInt", BInt(i)))
      case BList(l) => Failure(typeErr("BList", BList(l)))
      case BMap(m) => Failure(typeErr("BMap", BMap(m))) } }

  def intify(b: BDecoding): Try[Int] = {
    val typeErr = curryTypeErr("intify", "BInt")_
    b match {
      case BInt(i) => Success(i)
      case BStr(s) => Failure(typeErr("BStr", BStr(s)))
      case BList(l) => Failure(typeErr("BList", BList(l)))
      case BMap(m) => Failure(typeErr("BMap", BMap(m))) } }

  def lookup(b: BDecoding, keys: List[BKey]): Try[BDecoding] = b match {
    case BMap(bm) =>
      val keyErr = curryKeyErr("BMap") _
      keys match {
        case Nil => Success(b) // base case
        case Blk(i) :: tail => Failure(keyErr("BInt", Blk(i))) // error
        case Bmk(s) :: tail => lookup(bm(BStrify(s)), tail) // recur
      }
        case BList(bl) =>
      val keyErr = curryKeyErr("BList") _
      keys match {
        case Nil => Success(b) // base case
        case Bmk(s) :: tail => Failure(keyErr("BStr", Bmk(s))) // error
        case Blk(i) :: tail => lookup(bl(i), tail) // recur
      }
    case BStr(s) => Success(BStr(s))
    case BInt(i) => Success(BInt(i))
  }

  private def curryTypeErr(method: String, expectedType: String)(actualType: String, actualObject: BDecoding): Exception = {
    new Exception("BDecoding#" + method + " expects a " + expectedType +
      " but received the following " + actualType + ": " + actualObject.toString)
  }
  private def curryKeyErr(_type: String)(actKeyStr: String, actKey: BKey): Exception = {
    val expectedKey = if (_type == "BMap") "BStr" else "Int"
    new Exception("BDecoding#lookup on a" + _type + " requires a " + expectedKey + " key" +
      ", but was provided the following " + actKeyStr + " as a key: " + actKey)
  }
}
