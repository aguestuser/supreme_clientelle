package supreme_clientelle

import java.nio.file.{DirectoryStream, Files, Path, Paths}

/**
 * Created by aguestuser on 1/14/15.
 */

//////////////////////////
// for a config file    //
//////////////////////////

trait Env extends Enumeration {
  type Env = Value
  val Testing, Dev, Prod = Value
}

trait Port extends Enumeration {
  type Port = Value
  val 6881, 6882, 6883, 6884, 6885, 6886, 6887, 6888, 6889 = Value
}

trait Config {
  val env: Env
  val port: Int
  val srcPath: String
  val compact: Boolean
  val noPeerId: Boolean
}

object Config extends Env {
  val env = Testing
  val port = 6881
  val srcPath = env match {
    case `Testing` => "hmm/sample_data/torrent_queue"
    case _ => "hmm/torrent_queue"
  }
  val compact = true
    // -> some trackers will reject if set false
    // -> if true replaces peers list with peers ByteString
    //    * first 4 bytes are host
    //    * nest 2 bytes are ports
  def stringifyBool(bool: Boolean) : String = bool match {
      case true => "1"
      case false => "0"
  }
}
///////////////////////////////////////

///////////////////////////////////////
// for a TorrentManager or DAO file //
///////////////////////////////////////

trait TorrentStatus extends Enumeration {
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
  def getTorrentStates(bs: List[BDecoding], ds: List[TorrentState]) : List[TorrentState] = List(new TorrentState) // TODO fix!
  def updateState(old: TorrentState, _new: TorrentState) = new TorrentState // TODO fix!
}

///////////////////////////////////////

object Main extends App {

  import Config._; import TorrentManager._

  val metaInfoPaths : DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(srcPath))
  val metaInfoFiles : List[List[Byte]] =
    for ( path <- metaInfoPaths) yield Files.readAllBytes(path).toList
  val metaInfoDictionaries : List[BDecoding] = metaInfoFiles.map(BCodr.decode).toList

  val downloadPaths : DirectoryStream[Path] = {}
  val downloadFiles : List[List[Byte]] = {}
  val downloadStatuses : List[TorrentState] = {}

  val states = getTorrentStates(metaInfoDictionaries, downloadStatuses)

  val peerLists =
    (metaInfoFiles zip metaInfoDictionaries zip states).map {
      case ((raw, map), state) => TrakTalkr.getPeers(raw.toArray, map, Config, state)
    }
}
