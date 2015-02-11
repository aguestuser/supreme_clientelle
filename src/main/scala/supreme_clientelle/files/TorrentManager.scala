package supreme_clientelle.files

import java.nio.file.{Files, Paths}

import supreme_clientelle.bencode.{BDecoding, MetaInfoAccessor, BCodr}
import supreme_clientelle.files.TorrentStatus.TorrentStatus
import BCodr._

import scala.collection.JavaConversions._


/**
 * Author: @aguestuser
 * Date: 1/19/15
 * License: GPLv2
 */

object TorrentStatus extends Enumeration {
  type TorrentStatus = Value
  val started, stopped, completed = Value
}

class TorrentState(
                    val infoHash: String,
                    val length: Int,
                    val left: Int,
                    val uploaded: Int = 0,
                    val downloaded: Int = 0,
                    val numWant: Int = 50,
                    val status: TorrentStatus = TorrentStatus.started)

object TorrentManager {

  def getMetaInfos(path: String) : List[BDecoding] =
    (getMetaInfoFiles _ andThen getMetaInfoMaps)(path)

  def getTorrentStates(bs: List[BDecoding]) : List[TorrentState] =
    bs.map(getTorrentState)

  def getTorrentState(b: BDecoding) : TorrentState = {
    val length = MetaInfoAccessor.getLength(b)
    val default = new TorrentState(MetaInfoAccessor.getInfoHash(b), length, length)
    // TODO get current state based on download status
    // val updated = updateTorrentState(default, somePath)
    default
  }

  def updateTorrentState(old: TorrentState, path: String) = new TorrentState("hmm", 0, 0) // TODO fix!

  def getDownloadPath(metaPath: String) : String = "hmmm" // TODO fix!


  private def getMetaInfoFiles(path: String) : List[Array[Byte]] = {
   Files
     .newDirectoryStream(Paths.get(path))
     .toList
     .map { Files.readAllBytes }
  }

  private def getMetaInfoMaps(bls: List[Array[Byte]]) : List[BDecoding] = bls.map(decode)

}
