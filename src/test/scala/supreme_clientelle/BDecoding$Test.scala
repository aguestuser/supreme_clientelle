package supreme_clientelle

import org.specs2.mutable.Specification
import supreme_clientelle.BDecoding._

import scala.collection.immutable.ListMap

/**
 * Created by aguestuser on 1/14/15.
 */
class BDecoding$Test extends Specification {

  "BDecoding" in {

//    "#BStrify" should {
//      "compose a BStr from a String" in {
//        BStrify("hello") === BStr(List[Byte](104, 101, 108, 108, 111)) } }
//
//    "#strify" should {
//      "unpack a String from a BStr" in {
//        strify(BStr(List[Byte](104, 101, 108, 108, 111))) === "hello" } }
//
//    "#uNnest" should {
//
//      lazy val bm = BMap(ListMap(
//        BStrify("spams") -> BInt(2),
//        BStrify("eggs") -> BStrify("no!"),
//        BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
//        BStrify("breakfast map") -> BMap(ListMap(
//          BStrify("spams") -> BInt(2),
//          BStrify("dinner map") -> BMap(ListMap(
//            BStrify("eggs") -> BStrify("eat your veggies!")))))))
//
//      "unnest BDecodings from a BMap" in {
//
//        "for a BInt" in {
//          unNest(bm, List(Left(BStrify("spams")))) === Success(BInt(2)) }
//
//        "for a BStr" in {
//          unNest(bm, List(Left(BStrify("eggs")))) === Success(BStrify("no!")) }
//
//        "for a BList" in {
//          unNest(bm, List(Left(BStrify("meal counts")))) ===
//            Success(BList(List(BInt(1), BInt(2)))) }
//
//        "for a nested BInt" in {
//          unNest(bm, List(
//            Left(BStrify("breakfast map")),
//            Left(BStrify("spams")))) === Success(BInt(2)) }
//
//        "for a doubly-nested BStr" in {
//          unNest(bm, List(
//            Left(BStrify("breakfast map")),
//            Left(BStrify("dinner map")),
//            Left(BStrify("eggs")))) === Success(BStrify("eat your veggies!")) } } // returning "no!"
//
//
//      "un-nest BDecodings from a BList" in {
//
//        lazy val bl = BList(List(BStrify("one"), BList(List(BInt(2), BInt(3)))))
//
//        "for a BInt" in {
//          unNest(bl, List(Right(0))) === Success(BStrify("one")) }
//
//        "for a nested BStr" in {
//          unNest(bl, List(Right(1),Right(0))) === Success(BInt(2)) } }
//
//      "un-nest BDecodings for BMaps inside of BLists & vice versa" in {
//
//        lazy val bml = BMap(ListMap(
//          BStrify("one") ->
//            BList(List(BInt(2),BInt(3)))))
//
//        lazy val blm = BList(List(
//          BStrify("one"),
//          BMap(ListMap(BStrify("two") -> BStrify("three")))))
//
//        "for a BInt in a BList in a BMap" in {
//          unNest(bml, List(Left(BStrify("one")),Right(0))) === Success(BInt(2)) }
//
//        "for a BStr in a BMap in a BList" in {
//          unNest(blm, List(Right(1),Left(BStrify("two")))) === Success(BStrify("three")) } }
//
//      "handle errors" in {
//
//        "with incorrect Types" in {
//
//          "when passed a BInt" in {
//            unNest(BStrify("hello"), List(Right(0))) must
//              beFailedTry.withThrowable[Exception]("""\QBDecoding#unNest expects a BMap or BList but received the following BStr: BStr(List(104, 101, 108, 108, 111))\E""")
//          }
//        }
//      }
//    }

    "pretty unNesters" should {

      lazy val bm = BMap(ListMap(
        BStrify("spams") -> BInt(2),
        BStrify("eggs") -> BStrify("no!"),
        BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
        BStrify("breakfast map") -> BMap(ListMap(
          BStrify("spams") -> BInt(2),
          BStrify("dinner map") -> BMap(ListMap(
            BStrify("eggs") -> BStrify("eat your veggies!")))))))

      "#unNestAnd works" in {

        unNestAnd(Left(strify))(bm, List(
          Bmk("breakfast map"),
          Bmk("dinner map"),
          Bmk("eggs")
        )).left.get === "eat your veggies!"
      }

      "#unNestCurry works" in {
        unNestCurry(bm)
        Bmk("breakfast map")
        Bmk("dinner map")
        Bmk("eggs")
        EmptyBKey(()) === "eat your veggies!"

      }


      "Nested Indexer works" in {
        BStringEmpty.find("breakfast map")("dinner map")("eggs").in(bm)(strify) === "eat your veggies!"
      }

    }

  }
}
