package supreme_clientelle.bencode

import org.specs2.mutable.Specification
import supreme_clientelle.Util
import supreme_clientelle.bencode.BCodr._
import supreme_clientelle.bencode.BDecoding._

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
        decode("3:foo") === BStrify("foo")
      }
      "string with spaces" in {
        decode("20:austin spencer guest") === BStrify("austin spencer guest")
      }
      "string with every numerical digit" in {
        decode("10:1234567890") === BStrify("1234567890")
      }
      "string of non-character bytes" in {
        decode(List[Byte](53, 58, -35, -82, -8, -119, -111)) ===
          BStr(List[Byte](-35, -82, -8, -119, -111))
      }
    }

    "correctly decode LISTS" in {

      "simple list" in {
        decode("li2e4:spam3:egge") ===
          BList(List(BInt(2), BStr("spam".getBytes.toList), BStr("egg".getBytes.toList)))
      }
      "nested list" in {
        decode("li2e4:spam3:eggli2e4:spam3:eggee") ===
          BList(List(BInt(2), BStr("spam".getBytes.toList), BStr("egg".getBytes.toList),
            BList(List(BInt(2), BStr("spam".getBytes.toList), BStr("egg".getBytes.toList)))))
      }
      "empty list" in {
        decode("le") === BList(List[BDecoding]())
      }
    }

    "correctly decode MAPS" in {

      "simple map" in {
        decode("d3:cow3:moo4:spam4:eggs3:numi3ee") ===
          BMap(ListMap(
            BStr("cow".getBytes.toList) -> BStr("moo".getBytes.toList),
            BStr("spam".getBytes.toList) -> BStr("eggs".getBytes.toList),
            BStr("num".getBytes.toList) -> BInt(3)
          ))
      }
      "map with nested list" in {
        decode("d4:spaml1:a1:bee") ===
          BMap(ListMap(
            BStr("spam".getBytes.toList) -> BList(List(
              BStr("a".getBytes.toList), BStr("b".getBytes.toList)))))
      }
      "map with nested map" in {
        decode("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeee") ===
          BMap(ListMap(
            BStr("publisher".getBytes.toList) -> BStr("bob".getBytes.toList),
            BStr("publisher-details".getBytes.toList) ->
              BMap(ListMap(
                BStr("publisher-webpage".getBytes.toList) -> BStr("www.example.com".getBytes.toList),
                BStr("publisher.location".getBytes.toList) -> BStr("home".getBytes.toList),
                BStr("isbns".getBytes.toList) ->
                  BList(List(BInt(1), BInt(2), BInt(3)))))))
      }
      "empty map" in {
        decode("de") === BMap(ListMap[BStr,BDecoding]())
      }
    }
  }

  "BCodr.encode" should {

    "correctly encode INTS" in {

      "a positive int" in { encode(BInt(123)) === "i123e".getBytes.toList }
      "a negative int" in { encode(BInt(-123)) === "i-123e".getBytes.toList }
      "zero" in { encode(BInt(0)) === "i0e".getBytes.toList }
      "int with every digit and a leading 0" in {
        encode(BInt(123456789)) === "i123456789e".getBytes.toList
      }
    }
    "correctly encode STRINGS" in {
      
      "simple string" in {
        encode(BStrify("foo")) === "3:foo".getBytes.toList
      }
      "string with spaces" in {
        encode(BStrify("austin spencer guest")) === "20:austin spencer guest".getBytes.toList
      }
      "string with every numerical digit" in {
        encode(BStrify("1234567890")) === "10:1234567890".getBytes.toList
      }
      "string of non-character bytes" in {
        encode(BStr(List[Byte](-35, -82, -8, -119, -111))) ===
          List[Byte](53, 58, -35, -82, -8, -119, -111)
      }
    }
    "correctly encode LISTS" in  {
      
      "simple list" in {
        encode(BList(List(BInt(2), BStrify("spam"), BStrify("egg")))
        ) === "li2e4:spam3:egge".getBytes.toList
      }
      "nested list" in {
        encode(BList(List(
          BInt(2),
          BStrify("spam"),
          BStrify("egg"),
          BList(List(BInt(2), BStrify("spam"), BStrify("egg")))))
        ) === "li2e4:spam3:eggli2e4:spam3:eggee".getBytes.toList
      }
      "empty list" in {
        encode(BList(List())) === "le".getBytes.toList
      }      
    }
    "correctly encode MAPS" in {

      "simple map" in {

        encode(BMap(ListMap(
          BStr("cow".getBytes.toList) -> BStr("moo".getBytes.toList),
          BStr("spam".getBytes.toList) -> BStr("eggs".getBytes.toList),
          BStr("num".getBytes.toList) -> BInt(3)))
        ) === "d3:cow3:moo4:spam4:eggs3:numi3ee".getBytes.toList
      }
      "map with nested list" in {

        encode(BMap(ListMap(
          BStr("spam".getBytes.toList) -> BList(List(
            BStr("a".getBytes.toList), BStr("b".getBytes.toList)))))
        ) === "d4:spaml1:a1:bee".getBytes.toList
      }
      "map with nested map" in {

        encode(
          BMap(ListMap(
            BStr("publisher".getBytes.toList) -> BStr("bob".getBytes.toList),
            BStr("publisher-details".getBytes.toList) ->
              BMap(ListMap(
                BStr("publisher-webpage".getBytes.toList) -> BStr("www.example.com".getBytes.toList),
                BStr("publisher.location".getBytes.toList) -> BStr("home".getBytes.toList),
                BStr("isbns".getBytes.toList) ->
                  BList(List(BInt(1), BInt(2), BInt(3)))))))
        ) === ("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeee").getBytes.toList
      }
      "empty map" in {

        encode(BMap(ListMap[BStr,BDecoding]())) === "de".getBytes.toList
      }
    }
  }

}
