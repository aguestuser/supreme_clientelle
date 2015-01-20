package supreme_clientelle
import supreme_clientelle.BDecoding._
import supreme_clientelle.config.Config

/**
* Created by aguestuser on '1'/'1''4'/'1''5'.
*/

object TrakrTalkr {

  def buildRequests(rawFiles: List[List[Byte]],
                       bMaps: List[BDecoding],
                       states: List[TorrentState]): List[String] = {
    (rawFiles zip bMaps zip states).map {
      case ((raw, map), state) => buildRequest(raw.toArray, map, state)
    }
  }

  def buildRequest(raw: Array[Byte], bInfo: BDecoding, state: TorrentState) : String = {
    val url: String = lookupAndStringify(bInfo, List(Bmk("announce")))
    val params = getParams(raw, bInfo, state)
    formatRequest(url, params)
  }

  private def getParams(raw: Array[Byte], bInfo: BDecoding,state: TorrentState) = {
    List[(String,String)](
      ("info_hash", (hash _ andThen escape)(raw)),
      ("peer_id", "-AG0000-" + (hash _ andThen escape)("seed".getBytes).drop(6)),
      ("port", Config.port.toString),
      ("uploaded", state.uploaded.toString),
      ("downloaded", state.downloaded.toString),
      ("left", state.left.toString),
      ("compact", Config.stringifyBool(Config.compact)),
      ("no_peer_id", Config.stringifyBool(Config.noPeerId)),
      ("event", state.status.toString),
      ("numwant", state.numWant.toString),
//      ("trackerid", ""),
      ("length", lookupAndIntify(bInfo, List(Bmk("info"), Bmk("length"))).toString)
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
    url + "?" + params.map({case (x:String,y:String) => x + "=" + y}).mkString("&")
  }
}
