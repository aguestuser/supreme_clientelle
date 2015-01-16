package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.TrakTalkr._

/**
 * Created by aguestuser on 1/15/15.
 */
class TrakTalkr$Test extends Specification {
  "#escape" should {
    lazy val ba = Array[Byte](18, 52, 86, 120, -102, -68, -34, -15, 35, 69, 103, -119, -85, -51, -17, 18, 52, 86, 120, -102)
    //ie: \x12\x34\x56\x78\x9a\xbc\xde\xf1\x23\x45\x67\x89\xab\xcd\xef\x12\x34\x56\x78\x9a

    "escape a byte array with non-character bytes" in {
      escape(ba) === "%124Vx%9a%bc%de%f1%23Eg%89%ab%cd%ef%124Vx%9a"
    }

    "compose with #hash correctly" in {
      (hash _ andThen escape)(ba) === "%c5%85%16%3e%8a%ca%85PZ792%27%db%c6%c8X%9c%17%3d"
    }
  }
}
