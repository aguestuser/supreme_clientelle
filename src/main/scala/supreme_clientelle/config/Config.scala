package supreme_clientelle.config

/**
 * Created by aguestuser on 1/19/15.
 */

object Env extends Enumeration {
  type Env = Value
  val Testing, Dev, Prod = Value
}

//object Port extends Enumeration {
//  type Port = Value
//  val 6881, 6882, 6883, 6884, 6885, 6886, 6887, 6888, 6889 = Value
//}

object Config {
  val env = Env.Testing
  val port = 6881 // permitted: {6881, 6882, 6883, 6884, 6885, 6886, 6887, 6888, 6889 }
  val torrentQueue = env match {
    case Env.Testing => "/Users/aguestuser/code/hackerschool/supreme_clientelle/src/test/resources/torrent_queue"
    case _ => "../../../resources/torrent_queue"
    // eventually torrent que path for Dev/Prod
    // could be hard-coded based on app root derived form install?
  }
  val noPeerId = false
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


// alt design: store configs in a YAML file
// located in a Path provided to the below constructor
//trait Config(value: Path){
//  def generate = Source.io.getFrom
//}
//case class Testing(value: Path) extends Config
//case class Dev(value: Path) extends Config
//case class Prod(value: Path) extends Config
