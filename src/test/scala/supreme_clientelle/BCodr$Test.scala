package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.BCodr._

import scala.collection.immutable.ListMap

/**
 * Created by aguestuser on 1/9/15.
 */

class BCodr$Test extends Specification with Util {
  
  "BCodr.decode" should {

    "correctly decode INTS" in {

      "a positive int" in { decode("i123e") === BInt(123)}
      "a negative int" in { decode("i-123e") === BInt(-123)}
      "zero" in { decode("i0e") === BInt(0)}
      "int with every digit and a leading 0" in { decode("i0123456789e") === BInt(123456789)}
    }

    "correctly decode STRINGS" in {

      "simple string" in {
        decode("3:foo") === BStr("foo".getBytes.toList)
      }
      "string with spaces" in {
        ( decode("20:austin spencer guest") match { case BStr(s) => BStr(s).is } ) ===
          BStr("austin spencer guest".getBytes.toList)
      }
      "string with every numerical digit" in {
        ( decode("10:1234567890") match { case BStr(s) => BStr(s).is } )  ===
          BStr("1234567890".getBytes.toList)
      }
      "string of non-character bytes" in {
        ( decode(List[Byte](53, 58, -35, -82, -8, -119, -111)) match { case BStr(s) => BStr(s).is } ) ===
          BStr(List[Byte](-35, -82, -8, -119, -111)).is
      }
    }

    "correctly decode LISTS" in {

      "simple list" in {
        ( decode("li2e4:spam3:egge") match { case BList(l) => BList(l).is } ) ===
          BList(List(BInt(2), BStr("spam".getBytes.toList), BStr("egg".getBytes.toList)))
      }
      "nested list" in {
        ( decode("li2e4:spam3:eggli2e4:spam3:eggee") match { case BList(l) => BList(l).is } ) ===
          BList(List(BInt(2), BStr("spam".getBytes.toList), BStr("egg".getBytes.toList),
            BList(List(BInt(2), BStr("spam".getBytes.toList), BStr("egg".getBytes.toList)))))
      }
      "empty list" in {
        ( decode("le") match { case BList(l) => BList(l).is } ) ===
          BList(List[BDecoding]())
      }
    }

    "correctly decode MAPS" in {

      "simple map" in {
        ( decode("d3:cow3:moo4:spam4:eggs3:numi3ee") match { case BMap(m) => BMap(m).is } ) ===
          BMap(ListMap(
            BStr("cow".getBytes.toList) -> BStr("moo".getBytes.toList),
            BStr("spam".getBytes.toList) -> BStr("eggs".getBytes.toList),
            BStr("num".getBytes.toList) -> BInt(3)
          )).is
      }
      "map with nested list" in {
        ( decode("d4:spaml1:a1:bee") match { case BMap(m) => BMap(m).is } ) ===
          BMap(ListMap(
            BStr("spam".getBytes.toList) -> BList(List(
              BStr("a".getBytes.toList), BStr("b".getBytes.toList))))).is
      }
      "map with nested map" in {
        ( decode("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeeee") match { case BMap(m) => BMap(m).is } ) ===
          BMap(ListMap(
            BStr("publisher".getBytes.toList) -> BStr("bob".getBytes.toList),
            BStr("publisher-details".getBytes.toList) -> BMap(ListMap(
              BStr("publisher-webpage".getBytes.toList) -> BStr("www.example.com".getBytes.toList),
              BStr("publisher.location".getBytes.toList) -> BStr("home".getBytes.toList),
              BStr("isbns".getBytes.toList) -> BList(List(BInt(1), BInt(2), BInt(3)))
            )))).is
      }
      "empty map" in {
        ( decode("de") match { case BList(l) => BList(l).is } ) ===
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
