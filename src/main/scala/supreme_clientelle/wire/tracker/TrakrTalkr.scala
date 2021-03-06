package supreme_clientelle.wire.tracker

import dispatch.Defaults._
import dispatch._
import supreme_clientelle.Config
import supreme_clientelle.bencode.MetaInfoAccessor._
import supreme_clientelle.bencode._
import supreme_clientelle.files.TorrentState
import BCodr._

/**
* Created by aguestuser on 1/14/15.
*/

object TrakrTalkr {

  def buildRequests(bs: List[BDecoding], states: List[TorrentState], cfg: Config): List[Req] =
    for (b <- bs; s <- states) yield buildReq(b,s,cfg)

  def buildReq(b: BDecoding, state: TorrentState, cfg: Config) : Req = {
    val rb = new MyRequestBuilder; val _url = getAnnounceUrl(b); val params = getPars(b, state, cfg)
    rb.setUrl(formatRequest(_url, params)).GET
  }

  def getResponses(reqs: List[Req]): List[Future[BDecoding]] =
    reqs map { getResponse }

  def getResponse(req: Req) : Future[BDecoding] =
    Http(req OK as.Bytes) flatMap { r =>
      Future successful { decode(r.toVector) } }

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
    )
  }

  private def formatRequest(url: String, params: List[(String,String)]) : String =
    url + "?" + params.map({case (x:String,y:String) => x + "=" + y}).mkString("&")

}
