package supreme_clientelle

import supreme_clientelle.bencode.BDecoding
import supreme_clientelle.bencode.MetaInfoAccessor._



/**
* Created by aguestuser on '1'/'1''4'/'1''5'.
*/

object TrakrTalkr {

  def buildRequests(bMaps: List[BDecoding], states: List[TorrentState], cfg: Config): List[String] = {
    (bMaps zip states).map( { case (map, state) => buildRequest(map, state, cfg) } )
  }

  def buildRequest(bMap: BDecoding, state: TorrentState, cfg: Config) : String = {
    val url = getAnnounceUrl(bMap)
    val params = getParams(bMap, state, cfg)
    formatRequest(url, params)
  }

  private def getParams(bMap: BDecoding, state: TorrentState, cfg: Config) = {
    List[(String,String)](
      ("info_hash", getInfoHash(bMap)),
      ("peer_id",  getPeerIdHash(bMap)),
      ("port", cfg.port.toString),
      ("uploaded", state.uploaded.toString),
      ("downloaded", state.downloaded.toString),
      ("left", state.left.toString),
      ("compact", cfg.compactAsStr),
      ("no_peer_id", cfg.noPeerIdAsStr),
      ("event", state.status.toString),
      ("numwant", state.numWant.toString),
      ("length", getLength(bMap).toString)
      //      ("trackerid", "")
    )
  }

  private def formatRequest(url: String, params: List[(String,String)]) : String = {
    url + "?" + params.map({case (x:String,y:String) => x + "=" + y}).mkString("&")
  }

}
