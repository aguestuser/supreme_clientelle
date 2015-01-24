package supreme_clientelle
import dispatch._
import supreme_clientelle.bencode.MetaInfoAccessor._
import supreme_clientelle.bencode._

/**
* Created by aguestuser on 1/14/15.
*/

object TrakrTalkr {

  def buildRequests(bs: List[BDecoding], states: List[TorrentState], cfg: Config): List[Req] =
    for (b <- bs; s <- states) yield buildReq(b,s,cfg)

  def buildReq(b: BDecoding, state: TorrentState, cfg: Config) : Req = 
    url(getAnnounceUrl(b)) <<? getPars(b, state, cfg)
  
  private def getPars(b: BDecoding, state: TorrentState, cfg: Config) = {
    List[(String,String)](
      ("info_hash", getInfoHash(b)),
      ("peer_id", getPeerIdHash(b)),
      ("port", cfg.port.toString),
      ("uploaded", state.uploaded.toString),
      ("downloaded", state.downloaded.toString),
      ("left", state.left.toString),
      ("compact", cfg.compactAsStr),
      ("no_peer_id", cfg.noPeerIdAsStr),
      ("event", state.status.toString),
      ("numwant", state.numWant.toString),
      ("length", getLength(b).toString)
      //      ("trackerid", "")
    )
  }

  //  def buildReq(b: BDecoding, state: TorrentState, cfg: Config) : Req = {
  //    val _url = getAnnounceUrl(b); val params = getPars(b, state, cfg)
  //    url(formatRequest(_url, params))
  //  }
  //  
  //  private def formatRequest(url: String, params: List[(String,String)]) : String =
  //    url + "?" + params.map({case (x:String,y:String) => x + "=" + y}).mkString("&")

  // TODO dispatch is url-escaping my percent signs!!! how can i make it stop???
  // https://bhudgeons.telegr.am/blog_posts/handling-non-standard-urls-in-dispatch

}
