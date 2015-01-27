package supreme_clientelle.bencode

/**
 * Created by aguestuser on 1/27/15.
 */
trait ByteTools {
  def sum(bs: Array[Byte]) : Int = (0 /: bs)((sum,b) => 256*sum + (b & 0xff))
  def sum(bs: List[Byte]) : Int = (0 /: bs)((sum,b) => 256*sum + (b & 0xff))
  def strify(bs: Array[Byte]) = bs.map(_.toChar).mkString
  def strify(bs: List[Byte]) = bs.map(_.toChar).mkString
}
