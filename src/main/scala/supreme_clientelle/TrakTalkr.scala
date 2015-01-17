package supreme_clientelle
import supreme_clientelle.BDecoding._

/**
* Created by aguestuser on '1'/'1''4'/'1''5'.
*/

object TrakTalkr {

  def getPeers(rawInfo: Array[Byte], bInfo: BDecoding) : BDecoding = {
    val url: String = lookupAndStringify(bInfo, List(Bmk("announce")))
    val params = Map[String,String](
      "info_hash" -> (hash _ andThen escape)(rawInfo),
      "peer_id" -> ("-AG0000-" + (hash _ andThen escape)("seed".getBytes).drop(6)),
      "length" -> lookupAndStringify(bInfo, List(Bmk("info"), Bmk("length")))
    )
    BStrify("hmmm...")
  }

  def formatRequest(url: String, params: String) : String = {
    "hmm..."
  }

  def hash(bytes: Array[Byte]) : Array[Byte] =
    java.security.MessageDigest.getInstance("SHA-1").digest(bytes)


  def escape(bytes: Array[Byte]) : String = {
    val allowed = Set('0','1','2','3','4','5','6','7','8','9',
      'A','B','C','D','E','F','G','H','I','J','K','L','M',
      'N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
      'a','b','c','d','e','f','g','h','i','j','k','l','m',
      'n','o','p','q','r','s','t','u','v','w','x','y','z',
      '.','-','_','~').map(_.toByte)
    bytes.map(
      x => if (allowed.contains(x)) x.toChar.toString else "%" + x.toHexString.replace("ffffff","")
    ).mkString
  }
}
