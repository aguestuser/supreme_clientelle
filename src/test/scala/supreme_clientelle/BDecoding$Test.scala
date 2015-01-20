package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.BDecoding._

import scala.collection.immutable.ListMap
import scala.util.Success

/**
 * Created by aguestuser on 1/14/15.
 */
class BDecoding$Test extends Specification {

  "BDecoding" in {

    "#BStrify" should {
      "compose a BStr from a String" in {
        BStrify("hello") === BStr(List[Byte](104, 101, 108, 108, 111))
      }
    }

    "#stringify" should {
      "unpack a String from a BStr" in {
        stringify(BStr(List[Byte](104, 101, 108, 108, 111))) === Success("hello")
      }
    }

    "#lookup" should {

      lazy val bm = BMap(ListMap(
        BStrify("spams") -> BInt(2),
        BStrify("eggs") -> BStrify("no!"),
        BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
        BStrify("breakfast map") -> BMap(ListMap(
          BStrify("spams") -> BInt(2),
          BStrify("dinner map") -> BMap(ListMap(
            BStrify("eggs") -> BStrify("eat your veggies!")))))))

      "lookup BDecodings from a BMap" in {

        "for a BInt" in {
          lookup(bm, List(Bmk("spams"))) === Success(BInt(2)) }

        "for a BStr" in {
          lookup(bm, List(Bmk("eggs"))) === Success(BStrify("no!")) }

        "for a BList" in {
          lookup(bm, List(Bmk("meal counts"))) ===
            Success(BList(List(BInt(1), BInt(2)))) }

        "for a nested BInt" in {
          lookup(bm, List(
            Bmk("breakfast map"),
            Bmk("spams"))) === Success(BInt(2)) }

        "for a doubly-nested BStr" in {
          lookup(bm, List(
            Bmk("breakfast map"),
            Bmk("dinner map"),
            Bmk("eggs"))) === Success(BStrify("eat your veggies!"))
        }
      }

      "un-nest BDecodings from a BList" in {

        lazy val bl = BList(List(BStrify("one"), BList(List(BInt(2), BInt(3)))))

        "for a BInt" in {
          lookup(bl, List(Blk(0))) === Success(BStrify("one")) }

        "for a nested BStr" in {
          lookup(bl, List(Blk(1),Blk(0))) === Success(BInt(2)) } }

      "un-nest BDecodings for BMaps inside of BLists & vice versa" in {

        lazy val bml = BMap(ListMap(
          BStrify("one") ->
            BList(List(BInt(2),BInt(3)))))

        lazy val blm = BList(List(
          BStrify("one"),
          BMap(ListMap(BStrify("two") -> BStrify("three")))))

        "for a BInt in a BList in a BMap" in {
          lookup(bml, List(Bmk("one"),Blk(0))) === Success(BInt(2))
        }

        "for a BStr in a BMap in a BList" in {
          lookup(blm, List(Blk(1),Bmk("two"))) === Success(BStrify("three"))
        }
      }

      "handle errors" in {

        lazy val bm = BMap(ListMap(
          BStrify("spams") -> BInt(2),
          BStrify("eggs") -> BStrify("no!"),
          BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
          BStrify("breakfast map") -> BMap(ListMap(
            BStrify("spams") -> BInt(2),
            BStrify("dinner map") -> BMap(ListMap(
              BStrify("eggs") -> BStrify("eat your veggies!")))))))

        "with incorrect Keys" in {

          "when passed a BInt" in pending {
            lookup(bm, List(Blk(0))) must
              beFailedTry.withThrowable[Exception]("""BDecoding#lookup on aBMap requires a BStr key, but was provided the following BInt as a key: Blk(0))""")
          }
        }
      }
    }

    "lookup composed with type casts" should {

      lazy val bm = BMap(ListMap(
        BStrify("spams") -> BInt(2),
        BStrify("eggs") -> BStrify("no!"),
        BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
        BStrify("breakfast map") -> BMap(ListMap(
          BStrify("spams") -> BInt(2),
          BStrify("dinner map") -> BMap(ListMap(
            BStrify("eggs") -> BStrify("eat your veggies!")))))))

      "lookup and stringify a nested BStr" in {
        lookupAndStringify(bm, List(
          Bmk("breakfast map"),Bmk("dinner map"),Bmk("eggs")
        )) === "eat your veggies!"
      }

      "lookup and intify a nested BInt" in {
        lookupAndIntify(bm, List(
          Bmk("breakfast map"),Bmk("spams")
        )) === 2
      }

    }

    "BMapIndexer lookup" should {

      lazy val bm = BMap(ListMap(
        BStrify("spams") -> BInt(2),
        BStrify("eggs") -> BStrify("no!"),
        BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
        BStrify("breakfast map") -> BMap(ListMap(
          BStrify("spams") -> BInt(2),
          BStrify("dinner map") -> BMap(ListMap(
            BStrify("eggs") -> BStrify("eat your veggies!")))))))


//      "lookup strings with nice syntax" in {
//        StartLooking.find("breakfast map")("dinner map")("eggs").in(bm)(stringify) === "eat your veggies!"
//      }

    }

  }
}
