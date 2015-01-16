//package supreme_clientelle
//import supreme_clientelle.BDecoding._
//import supreme_clientelle.BCodr._
//
//import scala.util.Success
//
///**
// * Created by aguestuser on '1'/'1''4'/'1''5'.
// */
//object TrakrTalkr {
//  def getPeerList(rawInfo: Array[Byte], bInfo: BDecoding) : BDecoding = {
//    val url = unNestAndStrify(bInfo, List(Left(BStrify("announce"))))
//
//    val other_url = unNestAnd(strify)(bInfo, List(Left(BStrify("announce"))))
//    val params = Map[String,String](
//      "info_hash" -> (hash _ andThen escape)(rawInfo),
//      "peer_id" -> "-AG0000-" ++ (hash _ andThen escape)("seed".getBytes).drop(6),
//      "length" -> unNestAndStrify(bInfo, List(Left(BStrify("info")), Left(BStrify("length"))))
//    )
//
//    decode("hmmm...")
//  }
//
//  def formatRequest(url: String, params: String) : String = {
//
//  }
//
//  def hash(bytes: Array[Byte]) : Array[Byte] =
//    java.security.MessageDigest.getInstance("SHA-1").digest(bytes)
//
//
//  def escape(bytes: Array[Byte]) : String = {
//    val allowed = Set('0','1','2','3','4','5','6','7','8','9',
//      'A','B','C','D','E','F','G','H','I','J','K','L','M',
//      'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
//      '.','-','_','~').map(_.toByte)
//    bytes.map(
//      x => if (allowed.contains(x)) x.toChar.toString else "%%" + x.toInt.toHexString
//    ).mkString
//  }
//}
