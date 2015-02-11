package supreme_clientelle.bencode

import org.specs2.mutable.Specification
import supreme_clientelle.bencode.BCodr._
import supreme_clientelle.bencode.BDecoding._

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

      "simple string" in {
        decode("3:foo") === { BStrify("foo").deep }
      }
      "string with spaces" in {
        decode("20:austin spencer guest") === BStrify("austin spencer guest")
      }
      "string with every numerical digit" in {
        decode("10:1234567890") === BStrify("1234567890")
      }
      "string of non-character bytes" in {
        decode(Array[Byte](53, 58, -35, -82, -8, -119, -111)) ===
          BStr(Array[Byte](-35, -82, -8, -119, -111))
      }
    }

    "correctly decode LISTS" in {

      "simple list" in {
        decode("li2e4:spam3:egge") ===
          BList(List(BInt(2), BStr("spam".getBytes), BStr("egg".getBytes)))
      }
      "nested list" in {
        decode("li2e4:spam3:eggli2e4:spam3:eggee") ===
          BList(List(BInt(2), BStr("spam".getBytes), BStr("egg".getBytes),
            BList(List(BInt(2), BStr("spam".getBytes), BStr("egg".getBytes)))))
      }
      "empty list" in {
        decode("le") === BList(List[BDecoding]())
      }
    }

    "correctly decode MAPS" in {

      "simple map" in {
        decode("d3:cow3:moo4:spam4:eggs3:numi3ee") ===
          BMap(ListMap(
            BStr("cow".getBytes) -> BStr("moo".getBytes),
            BStr("spam".getBytes) -> BStr("eggs".getBytes),
            BStr("num".getBytes) -> BInt(3)
          ))
      }
      "map with nested list" in {
        decode("d4:spaml1:a1:bee") ===
          BMap(ListMap(
            BStr("spam".getBytes) -> BList(List(
              BStr("a".getBytes), BStr("b".getBytes)))))
      }
      "map with nested map" in {
        decode("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeee") ===
          BMap(ListMap(
            BStr("publisher".getBytes) -> BStr("bob".getBytes),
            BStr("publisher-details".getBytes) ->
              BMap(ListMap(
                BStr("publisher-webpage".getBytes) -> BStr("www.example.com".getBytes),
                BStr("publisher.location".getBytes) -> BStr("home".getBytes),
                BStr("isbns".getBytes) ->
                  BList(List(BInt(1), BInt(2), BInt(3)))))))
      }
      "empty map" in {
        decode("de") === BMap(ListMap[BStr,BDecoding]())
      }
    }
  }

  "BCodr.encode" should {

    "correctly encode INTS" in {

      "a positive int" in { encode(BInt(123)) === "i123e".getBytes }
      "a negative int" in { encode(BInt(-123)) === "i-123e".getBytes }
      "zero" in { encode(BInt(0)) === "i0e".getBytes }
      "int with every digit and a leading 0" in {
        encode(BInt(123456789)) === "i123456789e".getBytes
      }
    }
    "correctly encode STRINGS" in {
      
      "simple string" in {
        encode(BStrify("foo")) === "3:foo".getBytes
      }
      "string with spaces" in {
        encode(BStrify("austin spencer guest")) === "20:austin spencer guest".getBytes
      }
      "string with every numerical digit" in {
        encode(BStrify("1234567890")) === "10:1234567890".getBytes
      }
      "string of non-character bytes" in {
        encode(BStr(Array[Byte](-35, -82, -8, -119, -111))) ===
          Array[Byte](53, 58, -35, -82, -8, -119, -111)
      }
    }
    "correctly encode LISTS" in  {
      
      "simple list" in {
        encode(BList(List(BInt(2), BStrify("spam"), BStrify("egg")))
        ) === "li2e4:spam3:egge".getBytes
      }
      "nested list" in {
        encode(BList(List(
          BInt(2),
          BStrify("spam"),
          BStrify("egg"),
          BList(List(BInt(2), BStrify("spam"), BStrify("egg")))))
        ) === "li2e4:spam3:eggli2e4:spam3:eggee".getBytes
      }
      "empty list" in {
        encode(BList(List())) === "le".getBytes
      }      
    }
    "correctly encode MAPS" in {

      "simple map" in {

        encode(BMap(ListMap(
          BStr("cow".getBytes) -> BStr("moo".getBytes),
          BStr("spam".getBytes) -> BStr("eggs".getBytes),
          BStr("num".getBytes) -> BInt(3)))
        ) === "d3:cow3:moo4:spam4:eggs3:numi3ee".getBytes
      }
      "map with nested list" in {

        encode(BMap(ListMap(
          BStr("spam".getBytes) -> BList(List(
            BStr("a".getBytes), BStr("b".getBytes)))))
        ) === "d4:spaml1:a1:bee".getBytes
      }
      "map with nested map" in {

        encode(
          BMap(ListMap(
            BStr("publisher".getBytes) -> BStr("bob".getBytes),
            BStr("publisher-details".getBytes) ->
              BMap(ListMap(
                BStr("publisher-webpage".getBytes) -> BStr("www.example.com".getBytes),
                BStr("publisher.location".getBytes) -> BStr("home".getBytes),
                BStr("isbns".getBytes) ->
                  BList(List(BInt(1), BInt(2), BInt(3)))))))
        ) === ("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeee").getBytes
      }
      "empty map" in {

        encode(BMap(ListMap[BStr,BDecoding]())) === "de".getBytes
      }
    }
  }

}
