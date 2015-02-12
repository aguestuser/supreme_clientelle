package supreme_clientelle.bytes

import scala.util.parsing.input.{Position, Reader}

/**
 * Author: @aguestuser
 * Date: 1/27/15
 * License: GPLv2
 */

object ByteTools {

  val DIGITS = Set[Byte]('-','0','1','2','3','4','5','6','7','8','9')
  val CHARS = Set[Byte]('0','1','2','3','4','5','6','7','8','9','.','-','_','~',
    'A','B','C','D','E','F','G','H','I','J','K','L','M', 'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
    'a','b','c','d','e','f','g','h','i','j','k','l','m', 'n','o','p','q','r','s','t','u','v','w','x','y','z')

  def sum(bs: Vector[Byte]) : Int = (0 /: bs)((sum,b) => 256*sum + (b & 0xff))
  def strify(bs: Vector[Byte]) = bs.map(_.toChar).mkString
  def byteVector(str: String) = str.getBytes.toVector

  def hash(bytes: Vector[Byte]) : Vector[Byte] =
    java.security.MessageDigest.getInstance("SHA-1").digest(bytes.toArray).toVector

  def escape(bytes: Vector[Byte]) : String =
    bytes.map(charifyOrHexify).map(doubleDigitize).mkString

  private def charifyOrHexify(b: Byte) : String =
    if (CHARS.contains(b)) b.toChar.toString else hexify(b)

  private def hexify(b: Byte) : String = "%" + (b & 0xff).toHexString.toUpperCase

  private def doubleDigitize(hex: String) : String =
    """(%)([A-Z|0-9])(%|$)""".r.replaceAllIn(hex, m => (m group 1) + "0" + (m group 2) )

}

case class ByteReader(bs: Vector[Byte], at: Int) extends Reader[Byte] {
  def first = bs.head
  def rest = ByteReader(bs.tail, at + 1)
  def pos = BytePosition(at, bs.tail)
  def atEnd = bs.isEmpty
}

case class BytePosition(col: Int, bytes: Vector[Byte]) extends Position {
  def column = col
  def line = 1
  def lineContents = bytes.mkString
}