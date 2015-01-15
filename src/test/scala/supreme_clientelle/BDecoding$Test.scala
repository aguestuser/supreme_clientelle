package supreme_clientelle

import org.specs2.mutable.Specification
import scala.collection.immutable.ListMap
import supreme_clientelle.BDecoding._

import scala.util.Success

/**
 * Created by aguestuser on 1/14/15.
 */
class BDecoding$Test extends Specification {

  "BDecoding" in {

    "#BStrify" should {
      "compose a BStr from a String" in {
        BStrify("hello") === BStr("hello".getBytes.toList) } }

    "#strify" should {
      "unpack a String from a BStr" in {
        strify(BStr("hello".getBytes.toList)) === "hello" } }

    "#uNnest" should {

      lazy val bm = BMap(ListMap(
        BStrify("spams") -> BInt(2),
        BStrify("eggs") -> BStrify("no!"),
        BStrify("meal counts") -> BList(List(BInt(1),BInt(2))),
        BStrify("breakfast map") -> BMap(ListMap(
          BStrify("spams") -> BInt(2),
          BStrify("dinner map") -> BMap(ListMap(
            BStrify("eggs") -> BStrify("eat your veggies!")))))))

      "unnest BDecodings from a BMap" in {

        "for a BInt" in {
          unNest(bm, List(Left(BStrify("spams")))) === Success(BInt(2)) }

        "for a BStr" in {
          unNest(bm, List(Left(BStrify("eggs")))) === Success(BStrify("no!")) }

        "for a BList" in {
          unNest(bm, List(Left(BStrify("meal counts")))) ===
            Success(BList(List(BInt(1), BInt(2)))) }

        "for a nested BInt" in {
          unNest(bm, List(
            Left(BStrify("breakfast map")),
            Left(BStrify("spams")))) === Success(BInt(2)) }

        "for a doubly-nested BStr" in {
          unNest(bm, List(
            Left(BStrify("breakfast map")),
            Left(BStrify("dinner map")),
            Left(BStrify("eggs")))) === Success(BStrify("eat your veggies!")) } } // returning "no!"


      "un-nest BDecodings from a BList" in {

        lazy val bl = BList(List(BStrify("one"), BList(List(BInt(2), BInt(3)))))

        "for a BInt" in {
          unNest(bl, List(Right(0))) === Success(BStrify("one")) }

        "for a nested BStr" in {
          unNest(bl, List(Right(1),Right(0))) === Success(BInt(2)) } }

      "un-nest BDecodings for BMaps inside of BLists & vice versa" in {

        lazy val bml = BMap(ListMap(
          BStrify("one") ->
            BList(List(BInt(2),BInt(3)))))

        lazy val blm = BList(List(
          BStrify("one"),
          BMap(ListMap(BStrify("two") -> BStrify("three")))))

        "for a BInt in a BList in a BMap" in {
          unNest(bml, List(Left(BStrify("one")),Right(0))) === Success(BInt(2)) }

        "for a BStr in a BMap in a BList" in {
          unNest(blm, List(Right(1),Left(BStrify("two")))) === Success(BStrify("three")) } }

    }
  }
}
