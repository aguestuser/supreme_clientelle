package supreme_clientelle.bencode

import org.specs2.mutable.Specification
import supreme_clientelle.bencode.BCodr._
import supreme_clientelle.bencode.BDecoding._
import supreme_clientelle.bytes.ByteTools._

import scala.collection.immutable.ListMap

/**
 * Author: @aguestuser
 * Date: 1/9/15
 * License: GPLv2
 */

class BCodr$Test extends Specification {
  
  "BCodr" should {

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
        decode(Vector[Byte](53, 58, -35, -82, -8, -119, -111)) ===
          BStr(Vector[Byte](-35, -82, -8, -119, -111))
      }
    }

    "correctly decode LISTS" in {

      "simple list" in {
        decode("li2e4:spam3:egge") ===
          BList(List(BInt(2), BStrify("spam"), BStrify("egg")))
      }
      "nested list" in {
        decode("li2e4:spam3:eggli2e4:spam3:eggee") ===
          BList(List(BInt(2), BStr(byteVector("spam")), BStr(byteVector("egg")),
            BList(List(BInt(2), BStr(byteVector("spam")), BStr(byteVector("egg"))))))
      }
      "empty list" in {
        decode("le") === BList(List[BDecoding]())
      }
    }

    "correctly decode MAPS" in {

      "simple map" in {
        decode("d3:cow3:moo4:spam4:eggs3:numi3ee") ===
          BMap(ListMap(
            BStr(byteVector("cow")) -> BStr(byteVector("moo")),
            BStr(byteVector("spam")) -> BStr(byteVector("eggs")),
            BStr(byteVector("num")) -> BInt(3)
          ))
      }
      "map with nested list" in {
        decode("d4:spaml1:a1:bee") ===
          BMap(ListMap(
            BStr(byteVector("spam")) -> BList(List(
              BStr(byteVector("a")), BStr(byteVector("b"))))))
      }
      "map with nested map" in {
        decode("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeee") ===
          BMap(ListMap(
            BStr(byteVector("publisher")) -> BStr(byteVector("bob")),
            BStr(byteVector("publisher-details")) ->
              BMap(ListMap(
                BStr(byteVector("publisher-webpage")) -> BStr(byteVector("www.example.com")),
                BStr(byteVector("publisher.location")) -> BStr(byteVector("home")),
                BStr(byteVector("isbns")) ->
                  BList(List(BInt(1), BInt(2), BInt(3)))))))
      }
      "empty map" in {
        decode("de") === BMap(ListMap[BStr,BDecoding]())
      }
    }

    "correctly encode INTS" in {

      "a positive int" in { encode(BInt(123)) === byteVector("i123e") }
      "a negative int" in { encode(BInt(-123)) === byteVector("i-123e") }
      "zero" in { encode(BInt(0)) === byteVector("i0e") }
      "int with every digit and a leading 0" in {
        encode(BInt(123456789)) === byteVector("i123456789e")
      }
    }
    "correctly encode STRINGS" in {

      "simple string" in {
        encode(BStrify("foo")) === byteVector("3:foo")
      }
      "string with spaces" in {
        encode(BStrify("austin spencer guest")) === byteVector("20:austin spencer guest")
      }
      "string with every numerical digit" in {
        encode(BStrify("1234567890")) === byteVector("10:1234567890")
      }
      "string of non-character bytes" in {
        encode(BStr(Vector[Byte](-35, -82, -8, -119, -111))) ===
          Vector[Byte](53, 58, -35, -82, -8, -119, -111)
      }
    }
    "correctly encode LISTS" in  {

      "simple list" in {
        encode(BList(List(BInt(2), BStrify("spam"), BStrify("egg")))
        ) === byteVector("li2e4:spam3:egge")
      }
      "nested list" in {
        encode(BList(List(
          BInt(2),
          BStrify("spam"),
          BStrify("egg"),
          BList(List(BInt(2), BStrify("spam"), BStrify("egg")))))
        ) === byteVector("li2e4:spam3:eggli2e4:spam3:eggee")
      }
      "empty list" in {
        encode(BList(List())) === byteVector("le")
      }
    }
    "correctly encode MAPS" in {

      "simple map" in {

        encode(BMap(ListMap(
          BStrify("cow") -> BStrify("moo"),
          BStrify("spam") -> BStrify("eggs"),
          BStrify("num") -> BInt(3)))
        ) === byteVector("d3:cow3:moo4:spam4:eggs3:numi3ee")
      }

      "map with nested list" in {

        encode(BMap(ListMap(
          BStr(byteVector("spam")) -> BList(List(
            BStr(byteVector("a")), BStr(byteVector("b"))))))
        ) === byteVector("d4:spaml1:a1:bee")
      }

      "map with nested map" in {

        encode {
          BMap(ListMap(
            BStr(byteVector("publisher")) -> BStr(byteVector("bob")),
            BStr(byteVector("publisher-details")) ->
              BMap(ListMap(
                BStr(byteVector("publisher-webpage")) -> BStr(byteVector("www.example.com")),
                BStr(byteVector("publisher.location")) -> BStr(byteVector("home")),
                BStr(byteVector("isbns")) ->
                  BList(List(BInt(1), BInt(2), BInt(3)))))))
        } === byteVector("d9:publisher3:bob17:publisher-details" +
          "d17:publisher-webpage15:www.example.com18:publisher.location4:home5:isbns" +
          "li1ei2ei3eeee")
      }
      "empty map" in {
        encode(BMap(ListMap[BStr,BDecoding]())) === byteVector("de")
      }
    }
  }
}
