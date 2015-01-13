package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.BCodr._
import scala.collection.immutable.ListMap

/**
 * Created by aguestuser on 1/9/15.
 */
class BCodr$Test extends Specification {
  
  "BCodr.decode" should {

    "correctly decode INTS" in {

      "a positive int" in { decode("i123e") === BInt(123)}
      "a negative int" in { decode("i-123e") === BInt(-123)}
      "zero" in { decode("i0e") === BInt(0)}
      "int with every digit and a leading 0" in { decode("i0123456789e") === BInt(123456789)}
    }

    "correctly decode STRINGS" in {

      "simple string" in { decode("3:foo") === BStr("foo")}
      "string with spaces" in { decode("20:austin spencer guest") === BStr("austin spencer guest")}
      "string with every numerical digit" in { decode("10:1234567890") === BStr("1234567890")}
      "string with special characters" in pending{ true === true}
      "string with raw bytes" in pending{ true === true }
    }

    "correctly decode LISTS" in {

      "simple list" in {
        decode("li2e4:spam3:egge") === 
          BList(List(BInt(2), BStr("spam"), BStr("egg")))
      }
      "nested list" in {
        decode("li2e4:spam3:eggli2e4:spam3:eggee") === 
          BList(List(BInt(2), BStr("spam"), BStr("egg"),
            BList(List(BInt(2), BStr("spam"), BStr("egg")))))
      }
      "empty list" in {
        decode("le") === 
          BList(List[BDecoding]())
      }
    }

    "correctly decode MAPS" in {

      "simple map" in {
        decode("d3:cow3:moo4:spam4:eggs3:numi3ee") ===
          BMap(ListMap(
            BStr("cow") -> BStr("moo"),
            BStr("spam") -> BStr("eggs"),
            BStr("num") -> BInt(3)
          ))
      }
      "map with nested list" in {
        decode("d4:spaml1:a1:bee") ===
          BMap(ListMap(
            BStr("spam") -> BList(List(
              BStr("a"), BStr("b")))))
      }
      "map with nested map" in {
        decode("d9:publisher3:bob17:publisher-detailsd17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbnsli1ei2ei3eeeee") ===
          BMap(ListMap(
            BStr("publisher") -> BStr("bob"),
            BStr("publisher-details") -> BMap(ListMap(
              BStr("publisher-webpage") -> BStr("www.example.com"),
              BStr("publisher.location") -> BStr("home"),
              BStr("isbns") -> BList(List(BInt(1), BInt(2), BInt(3)))
            ))))
      }
      "empty map" in {
        decode("de") ===
          BMap(ListMap[BStr,BDecoding]())
      }
    }
  }

  "BCodr.encode" should {
    "correctly encode ints" in pending { true }
    "correctly encode strings" in pending { true }
    "correctly encode lists" in pending { true }
    "correctly encode maps" in pending { true }
  }

}
