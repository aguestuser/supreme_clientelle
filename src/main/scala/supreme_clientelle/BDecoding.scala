package supreme_clientelle

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

/**
 * Created by aguestuser on 1/14/15.
 */
sealed trait BDecoding
case class BStr(is: List[Byte]) extends BDecoding
case class BInt(is: Int) extends BDecoding
case class BList(is: List[BDecoding]) extends BDecoding
case class BMap(is: ListMap[BStr, BDecoding]) extends BDecoding

sealed trait BKey
case class Bmk(is: String) extends BKey
case class Blk(is: Int) extends BKey


object BDecoding extends BKey {

  def BStrify(str: String): BStr = BStr(str.getBytes.toList)

  def lookupAndStringify(b: BDecoding, keys: List[BKey]) : String = stringify(lookup(b,keys).get).get
  def lookupAndIntify(b: BDecoding, keys: List[BKey]) : Int = intify(lookup(b,keys).get).get

  def stringify(b: BDecoding): Try[String] = {
    val typeErr = curryTypeErr("strify", "BStr")_
    b match {
      case BStr(s) => Success(s.map(_.toChar).mkString)
      case BInt(i) => Failure(typeErr("BInt", BInt(i)))
      case BList(l) => Failure(typeErr("BList", BList(l)))
      case BMap(m) => Failure(typeErr("BMap", BMap(m)))
    }
  }

  def intify(b: BDecoding): Try[Int] = {
    val typeErr = curryTypeErr("intify", "BInt")_
    b match {
      case BInt(i) => Success(i)
      case BStr(s) => Failure(typeErr("BStr", BStr(s)))
      case BList(l) => Failure(typeErr("BList", BList(l)))
      case BMap(m) => Failure(typeErr("BMap", BMap(m)))
    }
  }

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

  // OO WAY TO DO LOOKUP
  // question: how to make this work for BMaps that branch to BLists (and take Ints as keys?)

  abstract class LookerUpper {
    def apply(key: String): LookerUpper = new LookerUpper(key, this)
    def run(b: BDecoding): Try[BDecoding] // abstract
    def listify: List[BKey] // abstract

    def in(b: BDecoding) = (transformer: BDecoding => Try[String]) => transformer(run(b).get).get
  }

  class KeepLooking(private val key: String, private val previous: KeepLooking) extends LookerUpper {
    def run(b: BDecoding): Try[BDecoding] = lookup(b, listify)
    def listify: List[BKey] = previous.listify :+ Bmk(key)
  }

  class StartLooking extends LookerUpper {
    def run(b: BDecoding) = Success(b) // base case
    def listify = List()
  }

  object StartLooking { // call to initialize lookup
    def find(key: String) = new StartLooking()(key) // first () calls apply
  }

  // usage: BstringEmpty.find("a key")(1)("another key").in(SomeBMap)(stringify)
}
