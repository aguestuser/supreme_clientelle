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


object BDecoding {

//  type BKey = Either[BStr, Int]
  type DoneOrKeepUnNesting = Either[Try[BDecoding], (BKey) => Either[Try[BDecoding], (BKey) => Try[BDecoding]]]
  type Unwrapper = Either[(BDecoding) => Try[String], (BDecoding) => Try[Int]]
  type Unwrapped = Either[String,Int]

  def BStrify(str: String): BStr = BStr(str.getBytes.toList)
  
  def strify(b: BDecoding): Try[String] = {
    val typeErr = curryTypeErr("strify", "BStr")_
    b match {
//      case BStr(s) => Success(strify(s)) // delegate to strify(List[Byte])
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

//  def unNest(b: BDecoding, keys: List[BKey]): Try[BDecoding] = b match {
//    case BMap(bm) =>
//      val keyErr = curryKeyErr("BMap") _
//      keys match {
//        case Nil => Success(b) // base case
//        case Right(i) :: tail => Failure(keyErr("BInt", Right(i))) // error
//        case Left(bs) :: tail => unNest(bm(bs), tail) // recur
//      }
//    case BList(bl) =>
//      val keyErr = curryKeyErr("BList") _
//      keys match {
//        case Nil => Success(b) // base case
//        case Left(bs) :: tail => Failure(keyErr("BStr", Left(bs))) // error
//        case Right(i) :: tail => unNest(bl(i), tail) // recur
//      }
//    case BStr(s) => Success(BStr(s)) // error
//    case BInt(i) => Success(BInt(i))
//  }

  def unNest(b: BDecoding, keys: List[BKey]): Try[BDecoding] = b match {
    case BMap(bm) =>
      val keyErr = curryKeyErr("BMap") _
      keys match {
        case Nil => Success(b) // base case
        case Blk(i) :: tail => Failure(keyErr("BInt", Blk(i))) // error
        case Bmk(s) :: tail => unNest(bm(BStrify(s)), tail) // recur
      }
    case BList(bl) =>
      val keyErr = curryKeyErr("BList") _
      keys match {
        case Nil => Success(b) // base case
        case Bmk(s) :: tail => Failure(keyErr("BStr", Bmk(s))) // error
        case Blk(i) :: tail => unNest(bl(i), tail) // recur
      }
    case BStr(s) => Success(BStr(s)) // error
    case BInt(i) => Success(BInt(i))
  }

  def unNestCurry(b: BDecoding) : (BKey) => DoneOrKeepUnNesting = unNestOne(b)_


//  def unNestOne(b: BDecoding)(s: String) : DoneOrKeepUnNesting = unNestOne(b)(BMapKey(s))
//  def unNestOne(b: BDecoding)(i: Int) : DoneOrKeepUnNesting = unNestOne(b)(BListKey(i))
  def unNestOne(b: BDecoding)(key: BKey) : DoneOrKeepUnNesting = b match {
    case BMap(bm) =>
      val keyErr = curryKeyErr("BMap") _
      key match {
        case EmptyBKey(k) => Left(Success(b)) // base case
        case Blk(i) => Left(Failure(keyErr("Int", Blk(i)))) // error
        case Bmk(s) => Right(unNestCurry(bm(BStrify(s)))) // recur
      }
    case BList(bl) =>
      val keyErr = curryKeyErr("BList") _
      key match {
        case EmptyBKey(k) => Left(Success(b)) // base case
        case Bmk(s) => Left(Failure(keyErr("String", Bmk(s)))) // error
        case Blk(i) => Right(unNestCurry(bl(i))) // recur
      }
    case BStr(s) => Left(Success(BStr(s))) // base case
    case BInt(i) => Left(Success(BInt(i))) // base case
  }

  // VERBOSE FP WAY

  def unNestAnd(unwrap: Unwrapper)(b: BDecoding, keys: List[BKey]): Unwrapped = unwrap match {
    case Left(f) => Left(f(unNest(b, keys).get).get)
    case Right(f) => Right(f(unNest(b, keys).get).get)
  }

//  def unNestAnd(unwrap: Unwrapper)(b: BDecoding, keys: List[BKey]): Unwrapped = unwrap match {
//    case Left(f) => Left(f(unNest(b, keys).get).get)
//    case Right(f) => Right(f(unNest(b, keys).get).get)
//  }

  private def curryTypeErr(method: String, expectedType: String)(actualType: String, actualObject: BDecoding): Exception = {
    new Exception("BDecoding#" + method + " expects a " + expectedType +
      " but received the following " + actualType + ": " + actualObject.toString)
  }
  private def curryKeyErr(_type: String)(actKeyStr: String, actKey: BKey): Exception = {
    val expectedKey = if (_type == "BMap") "BStr" else "Int"
    new Exception("BDecoding#unNest on a" + _type + " requires a " + expectedKey + " key" +
      ", but was provided the following " + actKeyStr + " as a key: " + actKey)
  }

  // OO WAY

  abstract class BMapIndexer {
    def apply(key: String): BMapIndexer = new BStringMapIndexer(key, this)
    def run(b: BDecoding): Try[BDecoding]
    def listify: List[BKey]
    def in(b: BDecoding) = (transformer: BDecoding => Try[String]) => transformer(run(b).get).get
  }

  object BStringEmpty {
    def find(key: String) = new BStringEmpty()(key)
  }

  class BStringEmpty extends BMapIndexer {
    def listify = List()
    def run(b: BDecoding) = Success(b)
  }

  class BStringMapIndexer(private val key: String, private val previous: BMapIndexer) extends BMapIndexer {
    def listify: List[BKey] = previous.listify :+ Bmk(key)
    def run(b: BDecoding): Try[BDecoding] = unNest(b, listify)
  }
}
