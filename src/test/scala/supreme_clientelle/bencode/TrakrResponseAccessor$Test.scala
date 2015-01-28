package supreme_clientelle.bencode

import org.specs2.mutable.Specification
import TrakrResponseAccessor._
import BDecoding._

import scala.collection.immutable.ListMap

/**
 * Created by aguestuser on 1/26/15.
 */
class TrakrResponseAccessor$Test extends Specification {

  "#getPeers" should {

    "retrieve correct peer list" in {

      "from a list of BMaps" in {

        lazy val bPeers = BList(List(BMap(ListMap(
          BStrify("peer id") -> BStrify("WHATEVER"),
          BStrify("host") -> BStr(List[Byte](74,90,-8,-72)),
          BStrify("port") -> BStr(List[Byte](26,-31))
        ))))

        getPeers(bPeers) === List(
          Peer(1247475896, 6881, None),
          Peer(1618897115, 63103, None)
        )
      }

      "from a BString" in {

        lazy val bPeers = BStr(List[Byte](74,90,-8,-72,26,-31,96,126,104,-37,-10,127))

        getPeers(bPeers) === List(
          Peer(1247475896, 6881, None),
          Peer(1618897115, 63103, None)
        )
      }
    }
  }
}
