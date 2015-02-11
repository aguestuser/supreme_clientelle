package supreme_clientelle

import org.specs2.mutable.Specification
import bencode._
import BDecoding._
import scala.collection.immutable.ListMap

/**
 * Created by aguestuser on 1/15/15.
 */
class TrakrTalkr$Test extends Specification {

  lazy val cfg: Config = new Config
  lazy val metaInfos = TorrentManager.getMetaInfos(cfg.torrentQueue)
  lazy val states = TorrentManager.getTorrentStates(metaInfos)

  "#buildRequests" should {

    lazy val req = TrakrTalkr.buildRequests(metaInfos, states, cfg).head

    "generate correct tracker request" in {

      req.toRequest.toString === "http://thomasballinger.com:6969/announce" +
        "?info_hash=%2B%15%CA%2B%FDH%CD%D7m9%ECU%A3%AB%1B%8AW%18%0A%09" +
        "&peer_id=-AG-0000-%02%DE%FE%D0k%CF4S%EBm%E6" +
        "&port=6881" +
        "&uploaded=0" +
        "&downloaded=0" +
        "&left=1277987" +
        "&compact=1" +
        "&no_peer_id=0" +
        "&event=started" +
        "&numwant=50" +
        "&length=1277987" +
        "\tGET"
    }

    "#getResponses" should {

      lazy val res = TrakrTalkr.getResponse(req)

      "get correct Byte Array response from Tracker" in {

        res must be_==(Array[Byte](100, 56, 58, 99, 111, 109, 112, 108, 101, 116, 101,
          105, 49, 101, 49, 48, 58, 100, 111, 119, 110, 108, 111, 97, 100, 101,
          100, 105, 49, 101, 49, 48, 58, 105, 110, 99, 111, 109, 112, 108, 101,
          116, 101, 105, 49, 101, 56, 58, 105, 110, 116, 101, 114, 118, 97, 108,
          105, 49, 56, 54, 49, 101, 49, 50, 58, 109, 105, 110, 32, 105, 110, 116,
          101, 114, 118, 97, 108, 105, 57, 51, 48, 101, 53, 58, 112, 101, 101, 114,
          115, 49, 50, 58, 74, 90, -8, -72, 26, -31, 96, 126, 104, -37, -10, 127, 101)).await

      }

      "yield correct BDecoding" in {

        val r = res.map { r => bencode.OldBCodr.decode(r.toList) }

        r must be_==(
          BMap(ListMap(
            BStrify("complete") -> BInt(1),
            BStrify("downloaded") -> BInt(1),
            BStrify("incomplete") -> BInt(1),
            BStrify("interval") -> BInt(1799), // varies -> how to test?
            BStrify("min interval") -> BInt(899), // varies -> how to test?
            BStrify("peers") ->
              BStr(List[Byte](74,90,-8,-72,26,-31,96,126,104,-37,-10,127))
          ))
        ).await
      }


    }
  }
}
