package supreme_clientelle

/**
 * Created by aguestuser on 1/14/15.
 */

object Main extends App {

  val metaInfoFiles = TorrentManager.getMetaInfoFiles
  val metaInfoDictionaries = TorrentManager.getMetaInfoDictionaries(metaInfoFiles)
  val states = TorrentManager.getTorrentStates(metaInfoDictionaries, List[TorrentState]())

  val trakrRequests = TrakrTalkr.buildRequests(metaInfoFiles, metaInfoDictionaries, states)


  // TODO add check for state of torrents in queue with something like:
  //  val downloadPaths : DirectoryStream[Path] = {}
  //  val downloadFiles : List[List[Byte]] = {}
  //  val downloadStatuses : List[TorrentState] = {}
  //
  //  val states = getTorrentStates(metaInfoDictionaries, downloadStatuses)

}
