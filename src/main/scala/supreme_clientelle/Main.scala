package supreme_clientelle

import java.nio.file.{DirectoryStream, Files, Path, Paths}

/**
 * Created by aguestuser on 1/14/15.
 */

trait ExecMode extends Enumeration {
  type ExecMode = Value
  val testing, dev, prod = Value
}

trait Cfg extends ExecMode {
  val mode = testing
  val srcPath = mode match {
    case `testing` => "hmm/sample_data/torrent_queue"
    case _ => "hmm/torrent_queue"
  }
}

object Main extends App with Cfg {
  val tPaths : DirectoryStream[Path] = Files.newDirectoryStream(Paths.get(srcPath))
  val tFiles : List[List[Byte]] =
    for ( path <- tPaths) yield Files.readAllBytes(path).toList
  val tMaps : List[BDecoding] = tFiles.map(BCodr.decode).toList
  val peerLists =
    (tFiles zip tMaps).map{ case (raw, map) => TrakTalkr.getPeers(raw.toArray, map) }
}
