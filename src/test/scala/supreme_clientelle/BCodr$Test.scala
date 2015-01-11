package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.BCodr._
import supreme_clientelle.BDecoding._

/**
 * Created by aguestuser on 1/9/15.
 */
class BCodr$Test extends Specification {

  object Encoded {
    lazy val ints = List("i123e", "i-123e", "i0e", "i0123456789e") // add leading 0 failure case?
    lazy val strings = List("3:foo", "20:austin spencer guest", "10:1234567890")
    lazy val lists = List(
      "li2e4:spam3:egge",
      "li2e4:spam3:eggli2e4:spam3:eggee",
      "le")
    lazy val maps = List(
      "d3:cow3:moo4:spam4:eggse",
      "d4:spaml1:a1:bee",
      "d9:publisher3:bob17:publisher-webpage15:www.example.com18:publisher.location4:homee",
      "de" ) 
  }
  
  object Decoded {
    lazy val ints = List(123, -123, 0, 123456789)
    lazy val strings = List("foo", "austin spencer guest", "1234567890")
    lazy val lists = List(
      List(2, "spam", "egg"),
      List(2, "spam", "egg", List(2, "spam", "egg")),
      List())
    lazy val maps = List(
      Map("cow" -> "moo", "spam" -> "eggs"),
      Map("spam" -> List("a", "b")),
      Map("publisher" -> "bob", "publisher-webpage" -> "www.example.com", "publisher.location" -> "home"),
      Map())
  }
  
  "#decode" should {
    "correctly decode ints" in {
      Encoded.ints.map(decode).map(unwrap) === Decoded.ints
    }
    "correctly decode strings" in {
      Encoded.strings.map(decode).map(unwrap) === Decoded.strings
    }
    "correctly decode lists" in {
      Encoded.lists.map(decode).map(unwrap) === Decoded.lists
    }
    "correctly decode maps" in {
      Encoded.maps.map(decode).map(unwrap) === Decoded.maps
    }
  }
}
