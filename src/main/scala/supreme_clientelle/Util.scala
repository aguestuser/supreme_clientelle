package supreme_clientelle

/**
 * Created by aguestuser on 1/13/15.
 */
trait Util {
//  def strToBytes(str: String) : List[Byte] = str.getBytes.toList
  def bytesToStr(bytes: List[Byte]) : String = bytes.map(_.toChar).mkString
  def bytesToStr(bytes: Array[Byte]) : String = bytes.map(_.toChar).mkString

}
