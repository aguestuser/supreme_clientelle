package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.BCodr._

/**
 * Created by aguestuser on 1/9/15.
 */
class BCodr$Test extends Specification {
  lazy val encoded = Map[String, List[String]](
    "ints" -> List[String]("i123e","i-123e", "i0e", "i0123456789e"),// add leading 0 failure case?
    "strings" -> List[String]("3:foo", "20:austin spencer guest", "10:1234567890"),
    "lists" -> List[String]("l4:spam3:egge")
  )
  lazy val decoded = Map(
    "ints" -> List[BInt](BInt(123), BInt(-123), BInt(0), BInt(123456789)),
    "strings" -> List[BStr](BStr("foo"), BStr("austin spencer guest"), BStr("1234567890")),
    "lists" -> List[BList](BList(List(BStr("spam"),BStr("egg"))))
  )

  "#decode" should {
    "correctly decode ints" in {
      encoded("ints").map{ decode }.map{ _.head } === decoded("ints") // TO-DO: why can't i call .map( _.is) on this!?
    }
    "correctly decode strings" in {
      encoded("strings").map{ decode }.map{ _.head } === decoded("strings")
    }
    "correctly decode lists" in {
      encoded("lists").map{ decode }.map{ _.head } === decoded("lists")
    }
  }
}
