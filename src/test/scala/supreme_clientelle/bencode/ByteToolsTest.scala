package supreme_clientelle.bencode

import supreme_clientelle.bytes.ByteTools._

import org.specs2.mutable.Specification


/**
 * Author: @aguestuser
 * Date: 1/27/15
 * License: GPLv2
 */

class ByteToolsTest extends Specification {

  "#sum" should {
    "sum a byte vector to an int" in {
      sum(Vector[Byte](74, 90, -8, -72)) === 1247475896
    }
  }
  "#strify" should {
    "convert a byte vector to a string" in {
      strify(byteVector("hello")) === "hello"
    }
  }
}
