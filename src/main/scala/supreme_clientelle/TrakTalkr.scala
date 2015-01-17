package supreme_clientelle
import supreme_clientelle.BDecoding._

/**
* Created by aguestuser on '1'/'1''4'/'1''5'.
*/

object TrakTalkr {

  def getPeers(raw: Array[Byte], bInfo: BDecoding, cfg: Config, state: TorrentState) : BDecoding = {
    val url: String = lookupAndStringify(bInfo, List(Bmk("announce")))
    val params = getParams(raw, bInfo, cfg, state)
    val request = formatRequest(url, params)
    // fire off a url request with a future here!
    BStrify("hmmm...")
  }

  def getParams(raw: Array[Byte], bInfo: BDecoding, cfg: Config, state: TorrentState) = {
    List[(String,String)](
      ("info_hash", (hash _ andThen escape)(raw)),
      ("peer_id", "-AG0000-" + (hash _ andThen escape)("seed".getBytes).drop(6)),
      ("port", cfg.port.toString),
      ("uploaded", state.uploaded.toString),
      ("downloaded", state.downloaded.toString),
      ("left", state.left.toString),
      ("compact", Config.stringifyBool(cfg.compact)),
      ("no_peer_id", Config.stringifyBool(cfg.noPeerId)),
      ("event", state.status.toString()),
      ("numwant", state.numWant.toString),
//      ("trackerid", ""),
      ("length", lookupAndStringify(bInfo, List(Bmk("info"), Bmk("length"))))
    )
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

  def formatRequest(url: String, params: List[(String,String)]) : String = {
    url + "?" + params.flatMap(case (x:String,y:String) => x + "=" + y + "&")
  }
}
