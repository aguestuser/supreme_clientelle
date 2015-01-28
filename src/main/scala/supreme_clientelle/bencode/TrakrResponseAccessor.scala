package supreme_clientelle.bencode

/**
 * Created by aguestuser on 1/26/15.
 */

case class Peer(host: Int, port: Int, id: Option[Int])

object TrakrResponseAccessor extends ByteTools {

  def getPeers(bd: BDecoding) : List[Peer] = bd match {
//    case BList(bl) => bl map { peerFromBMap }
    case BStr(bs) => peersFromBytes(bs)
    case bad => throw new Exception(
      "TrakrResponseAccessor#getPeers expects either a BList or or BStr but received " + bad.toString)
  }

//  def peerFromBMap(bm: BDecoding) = Peer(
//      lookup(bm, List(Bmk("host"))) match { case Success(BStr(b)) => sum(b) },
//      lookup(bm, List(Bmk("port"))) match { case Success(BStr(b)) => IPCodr.decode(b) },
//      Some(lookupAndIntify(bm, List(Bmk("peer id")))))

  private def peersFromBytes(bs: List[Byte]) : List[Peer] =
    partitionEvery(6)(bs) map { peerFromBytes }

  private def partitionEvery(span: Int)(bytes: List[Byte]) : List[List[Byte]] = bytes match {
    case Nil => List()
    case l if l.size < span => l :: List()
    case l => l.take(span) :: partitionEvery(span)(l.drop(span))
  }

  private def peerFromBytes(bs: List[Byte]) : Peer =
    portHostify(bs) match { case(h,p) => Peer(h,p, None) }

  private def portHostify(bytes: List[Byte]) : (Int, Int) =
    bytes.splitAt(4) match { case (h,p) => (sum(h), sum(p)) }

}
