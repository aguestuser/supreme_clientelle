package supreme_clientelle.bencode

/**
 * Created by aguestuser on 1/27/15.
 */

trait IPDecoding
case class IPv6(is: Int)
case class IPv4(is: Int)
case class DNS(is: Int)

//class IPCodr extends ByteTools {
//  def decode(bs: List[Byte]) : IPDecoding = decode(strify(bs))
//  def decode(str: String) : IPDecoding = str match {
//    case ipv6 if """([0-9]{1,3})"""
//  }
//
//}
