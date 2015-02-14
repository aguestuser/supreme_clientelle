package supreme_clientelle

import supreme_clientelle.bencode.BDecoding
import supreme_clientelle.files.TorrentManager._
import supreme_clientelle.wire.tracker.TrakrTalkr._

import scala.concurrent.Future

/**
 * Created by aguestuser on 1/14/15.
 */

object Main extends App {

  val cfg: Config = new Config
  val metaInfos = getMetaInfos(cfg.torrentQueue)
  val states = getTorrentStates(metaInfos)

  val peerLists: List[Future[BDecoding]] =
    getResponses(buildRequests(metaInfos, states, cfg))






}
