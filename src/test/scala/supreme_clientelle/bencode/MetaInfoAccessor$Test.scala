package supreme_clientelle.bencode

import org.specs2.mutable.Specification
import supreme_clientelle.{Config, TorrentManager}
import supreme_clientelle.bencode.MetaInfoAccessor._


/**
 * Created by aguestuser on 1/20/15.
 */
class MetaInfoAccessor$Test extends Specification {
  lazy val cfg = new Config
  lazy val metaInfo = TorrentManager.getMetaInfos(cfg.torrentQueue).head

  "#getAnnounceUrl" should {

    "get correct Announce URL" in {
      getAnnounceUrl(metaInfo) === "http://thomasballinger.com:6969/announce"
    }
  }

  "#getLength" should {

    "get correct length" in {
      getLength(metaInfo) === 1277987
    }
  }

  "#getInfoHash" should {

    "get correct info hash" in {
      getInfoHash(metaInfo) === "%2B%15%CA%2B%FDH%CD%D7m9%ECU%A3%AB%1B%8AW%18%0A%09"
    }
  }

  "#getPeerIdHash" should {

    "get correct peer id hash" in {
      getPeerIdHash(metaInfo) === "-AG0000-%FE%D0k%CF4S%EBm%E6144%E2%86%89e%BF%07"
    }
  }

  "#escape" should {
    lazy val ba = Array[Byte](18, 52, 86, 120, -102, -68, -34, -15, 35, 69, 103, -119, -85, -51, -17, 18, 52, 86, 120, -102)
    //ie: \x12\x34\x56\x78\x9a\xbc\xde\xf1\x23\x45\x67\x89\xab\xcd\xef\x12\x34\x56\x78\x9a

    "escape a byte array with non-character bytes" in {
      escape(ba) === "%124Vx%9A%BC%DE%F1%23Eg%89%AB%CD%EF%124Vx%9A"
    }

    "compose with #hash correctly" in {
      (hash _ andThen escape)(ba) === "%C5%85%16%3E%8A%CA%85PZ792%27%DB%C6%C8X%9C%17%3D"
    }
  }
}
