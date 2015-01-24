package supreme_clientelle

import org.specs2.mutable.Specification

/**
 * Created by aguestuser on 1/15/15.
 */
class TrakrTalkr$Test extends Specification {

  lazy val cfg: Config = new Config
  lazy val metaInfos = TorrentManager.getMetaInfos(cfg.torrentQueue)
  lazy val states = TorrentManager.getTorrentStates(metaInfos)
  lazy val expectedRequests = TrakrTalkr.buildRequests(metaInfos, states, cfg)
//  lazy val expectedResponses = TrakrTalkr.getResponses(expectedRequests)

  "#buildRequests" should {

    "generate correct tracker request" in {

      expectedRequests.head.toRequest === "http://thomasballinger.com:6969/announce" +
        "?info_hash=%2B%15%CA%2B%FDH%CD%D7m9%ECU%A3%AB%1B%8AW%18%0A%09" +
        "&peer_id=-AG0000-%FE%D0k%CF4S%EBm%E6144%E2%86%89e%BF%07" +
        "&port=6881" +
        "&uploaded=0" +
        "&downloaded=0" +
        "&left=1277987" +
        "&compact=1" +
        "&no_peer_id=0" +
        "&event=started" +
        "&numwant=50" +
        "&length=1277987" +
        "\tGET\theaders:"

      // TANSY'S WORKING REQUEST
      // http://thomasballinger.com:6969/announce
      // ?left=1277987
      // &info_hash=%2B%15%CA%2B%FDH%CD%D7m9%ECU%A3%AB%1B%8AW%18%0A%09
      // &downloaded=0
      // &event=started
      // &peer_id=-TZ-0000-00000000000
      // &port=6881
    }
  }

  "#getResponses" should {

    "get correct response from Tracker" in pending { true }

  }
}
