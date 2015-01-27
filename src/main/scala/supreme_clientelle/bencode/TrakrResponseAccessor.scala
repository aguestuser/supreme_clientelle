package supreme_clientelle.bencode

import supreme_clientelle.bencode.BDecoding._

/**
 * Created by aguestuser on 1/26/15.
 */

case class Peer(host: List[Byte], port: Int, id: Option[Int])

object TrakrResponseAccessor {

  def getPeers(bd: BDecoding) : List[Peer] = bd match {
    case BList(bl) => peersFromBMaps(bl)
    case BStr(bs) => peersFromBytes(bs)
    case _ => throw new Exception("whoops!")
  }

  def peersFromBMaps(bl: List[BDecoding]) : List[Peer] = {
    bl map { bm => Peer(
      lookup(bm, List(Bmk("host"))).get match { case BStr(b) => b },
      lookupAndIntify(bm, List(Bmk("port"))),
      Some(lookupAndIntify(bm, List(Bmk("peer id"))))
    )}
  }

  def peersFromBytes(b: List[Byte]) : List[Peer] =
    partitionEvery(6)(b) map { portHostify } map { case(h,p) => Peer(h,p, None) }

  private def partitionEvery(span: Int)(ints: List[Byte]) : List[List[Byte]] = ints match {
    case Nil => List()
    case l if l.size < span => l :: List()
    case l => l.take(span) :: partitionEvery(span)(l.drop(span))
  }

  private def portHostify(bytes: List[Byte]) : (List[Byte], Int) =
    bytes.splitAt(4) match { case (h,p) => (h, 256*unsignify(p(0)) + unsignify(p(1))) }

  private def unsignify(i: Int) : Int = if (i < 0) i + 256 else i

}
