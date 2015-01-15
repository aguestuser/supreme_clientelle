package supreme_clientelle

import scala.collection.immutable.ListMap
import scala.util.{Failure, Success, Try}

/**
 * Created by aguestuser on 1/14/15.
 */
sealed abstract class BDecoding
case class BStr(is: List[Byte]) extends BDecoding
case class BInt(is: Int) extends BDecoding
case class BList(is: List[BDecoding]) extends BDecoding
case class BMap(is: ListMap[BStr, BDecoding]) extends BDecoding

object BDecoding {

  type BKey = Either[BStr, Int]

  def BStrify(str: String): BStr = BStr(str.getBytes.toList)
  def strify(bs: BStr): String = bs.is.map(_.toChar).mkString
  def strify(bl: List[Byte]): String = bl.map(_.toChar).mkString
  def strify(ba: Array[Byte]): String = ba.map(_.toChar).mkString

  def toByteArray(b: BDecoding): Try[Array[Byte]] = {
    val typeErr = curryTypeErr("toByteArr", "BStr")_
    b match {
      case BStr(s) => Success(s.toArray)
      case BInt(i) => Failure(typeErr("BInt", BInt(i)))
      case BMap(m) => Failure(typeErr("BMap", BMap(m)))
      case BList(l) => Failure(typeErr("BList", BList(l)))
    }
  }

  def unNest(b: BDecoding, keys: List[BKey]): Try[BDecoding] = {
    val typeErr = curryTypeErr("unNest", "BMap or BList")_
    b match {
      case BMap(bm) =>
        val keyErr = curryKeyErr("BMap") _
        keys match {
          case Nil => Failure(noKeyErr) // error
          case Right(i) :: tail => Failure(keyErr("BInt", Right(i))) // error
          case Left(bs) :: Nil => Success(bm(bs)) // base case
          case Left(bs) :: tail => unNest(bm(bs), tail) // recur
        }
      case BList(bl) =>
        val keyErr = curryKeyErr("BList") _
        keys match {
          case Nil => Failure(noKeyErr) // error
          case Left(bs) :: tail => Failure(keyErr("BStr", Left(bs))) // error
          case Right(i) :: Nil => Success(bl(i)) // base case
          case Right(i) :: tail => unNest(bl(i), tail) // recur
        }
      case BStr(bs) => Failure(typeErr("BStr", BStr(bs))) // error
      case BInt(bi) => Failure(typeErr("BInt", BInt(bi))) // error
    }
  }

  private def curryTypeErr(method: String, expectedType: String)(actualType: String, actualObject: BDecoding): Exception = {
    new Exception("BDecoding#" + method + " expects a " + expectedType +
      " but received the following " + actualType + ": " + actualObject.toString)
  }
  private def curryKeyErr(_type: String)(actKeyStr: String, actKey: BKey): Exception = {
    val expectedKey = if (_type == "BMap") "BStr" else "Int"
    new Exception("BDecoding#unNest on a" + _type + " requires a " + expectedKey + " key" +
      ", but was provided the following " + actKeyStr + " as a key: " + actKey)
  }
  private def noKeyErr = {
    new Exception("BDecoding#unNest expects a List of BKeys, " +
      "but you provided an empty list")
  }
}
