package supreme_clientelle

import supreme_clientelle.files.TorrentManager
import supreme_clientelle.wire.tracker.TrakrTalkr

/**
 * Created by aguestuser on 1/14/15.
 */

object Main extends App {

  val cfg: Config = new Config
  val metaInfos = TorrentManager.getMetaInfos(cfg.torrentQueue)
  val states = TorrentManager.getTorrentStates(metaInfos)

  val trakrRequests = TrakrTalkr.buildRequests(metaInfos, states, cfg)

}
