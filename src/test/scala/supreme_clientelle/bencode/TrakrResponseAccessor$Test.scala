package supreme_clientelle.bencode

import org.specs2.mutable.Specification
import TrakrResponseAccessor._

/**
 * Created by aguestuser on 1/26/15.
 */
class TrakrResponseAccessor$Test extends Specification {

  "#getPeers" should {

    "retrieve correct peer list" in {

      "from a list of BMaps" in pending {true}

      "from a BString" in {

        lazy val bPeers = BStr(List[Byte](74,90,-8,-72,26,-31,96,126,104,-37,-10,127))

        getPeers(bPeers) === List(
          Peer(List[Byte](74,90,-8,-72), 6881, None),
          Peer(List[Byte](96,126,104,-37), 63103, None) // TODO is port 63103 possible?
        )
      }
    }
  }
}
