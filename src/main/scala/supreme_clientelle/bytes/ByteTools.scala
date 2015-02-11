package supreme_clientelle.bytes

import scala.util.parsing.input.{Position, Reader}

/**
 * Created by aguestuser on 1/27/15.
 */

object ByteTools {
  def sum(bs: Array[Byte]) : Int = (0 /: bs)((sum,b) => 256*sum + (b & 0xff))
  def sum(bs: List[Byte]) : Int = (0 /: bs)((sum,b) => 256*sum + (b & 0xff))
  def strify(bs: Array[Byte]) = bs.map(_.toChar).mkString
  def strify(bs: List[Byte]) = bs.map(_.toChar).mkString
}

case class ByteReader(bs: Array[Byte], at: Int) extends Reader[Byte] {
  def first = bs.head
  def rest = ByteReader(bs.tail, at + 1)
  def pos = BytePosition(at, bs.tail)
  def atEnd = bs.isEmpty
}

case class BytePosition(col: Int, bytes: Array[Byte]) extends Position {
  def column = col
  def line = 1
  def lineContents = bytes.mkString
}