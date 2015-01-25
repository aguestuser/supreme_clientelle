package supreme_clientelle

/**
 * Created by aguestuser on 1/19/15.
 */

class Config(
              val torrentQueue: String =
                "src/test/resources/torrent_queue",
<<<<<<< HEAD
            //TODO replace with relative path
=======
>>>>>>> d308182a4bc621f8de8209eec788cce76eb18dd8
              val port: Int =
                6881, // permitted: Set{ 6881, 6882, 6883, 6884, 6885, 6886, 6887, 6888, 6889 }
              val noPeerId: Boolean =
                false,
              val compact: Boolean =
                true
                // -> some trackers will reject if set false
                // -> if true replaces peers list with peers ByteString
                //    * first 4 bytes are host
                //    * nest 2 bytes are ports
              ) {
  def noPeerIdAsStr = stringifyBool(noPeerId)
  def compactAsStr = stringifyBool(compact)
  private def stringifyBool(bool: Boolean): String = if (bool) "1" else "0"
}


// alt design: store configs in a YAML file
// located in a Path provided to the below constructor
//trait Config(value: Path){
//  def generate = Source.io.getFrom
//}
//case class Testing(value: Path) extends Config
//case class Dev(value: Path) extends Config
//case class Prod(value: Path) extends Config
