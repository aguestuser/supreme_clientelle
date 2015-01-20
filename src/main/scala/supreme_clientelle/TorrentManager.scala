package supreme_clientelle

import java.nio.file.{Files, Paths}
import supreme_clientelle.TorrentStatus.TorrentStatus
import scala.collection.JavaConversions._

/**
 * Created by aguestuser on 1/19/15.
 */
object TorrentStatus extends Enumeration {
  type TorrentStatus = Value
  val started, stopped, completed = Value
}

class TorrentState(
                    val status: TorrentStatus = TorrentStatus.started,
                    val uploaded: Int = 0,
                    val downloaded: Int = 0,
                    val left: Int = 0,
                    val numWant: Int = 50)

object TorrentManager {

  def getTorrentStates(bs: List[BDecoding], ds: List[TorrentState]) : List[TorrentState] =
    bs.map( x => new TorrentState) // TODO fix!
  def updateTorrentState(old: TorrentState, _new: TorrentState) =
    new TorrentState // TODO fix!

  def getMetaInfoFiles : List[List[Byte]] = {
   val paths = Files.newDirectoryStream(Paths.get(supreme_clientelle.config.Config.torrentQueue)).toList
    paths.map(Files.readAllBytes(_).toList)
  }

  def getMetaInfoDictionaries(bls: List[List[Byte]]) : List[BDecoding] = bls.map(BCodr.decode).toList

}
