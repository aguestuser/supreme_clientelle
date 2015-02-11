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
    "sum a byte array to an int" in {
      sum(Array[Byte](74, 90, -8, -72)) === 1247475896
    }
    "sum a byte list to an int" in {
      sum(List[Byte](74, 90, -8, -72)) === 1247475896
    }
  }
  "#strify" should {
    "convert a byte array to a string" in {
      strify("hello".getBytes) === "hello"
    }
    "convert a byte list to a string" in {
      strify("hello".getBytes.toList) === "hello"
    }
  }
}
