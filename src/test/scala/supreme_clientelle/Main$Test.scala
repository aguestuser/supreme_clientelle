package supreme_clientelle

import org.specs2.mutable.Specification

/**
 * Created by aguestuser on 1/19/15.
 */
class Main$Test extends Specification {

  "Main loop" should {

    "generate correct tracker request" in {
      val metaInfoFiles = TorrentManager.getMetaInfoFiles
      val metaInfoDictionaries = TorrentManager.getMetaInfoDictionaries(metaInfoFiles)
      val states = TorrentManager.getTorrentStates(metaInfoDictionaries, List[TorrentState]())

      TrakrTalkr.buildRequests(metaInfoFiles,metaInfoDictionaries,states) === List(
        "http://thomasballinger.com:6969/announce?" +
          "info_hash=%c5%9c%b9%a0%27%2c%88%9c%c3y%f4%a6%1b%eT%b7%21%cf%ed%a2" +
          "&peer_id=-AG0000-dG%97q%11%cf1%f2%a7%19%86%c4%11%bdl%b5%b0" +
          "&port=6881" +
          "&uploaded=0" +
          "&downloaded=0" +
          "&left=0" + // for a new torrent, this should be = to length
          "&compact=1" +
          "&no_peer_id=0" +
          "&event=started" +
          "&numwant=50" +
          "&length=1277987")

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
}
