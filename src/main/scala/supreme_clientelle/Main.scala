package supreme_clientelle

import java.nio.file.{Files, Paths}
import supreme_clientelle.BCodr._

/**
 * Created by aguestuser on 1/14/15.
 */

object Main extends App {
  val byteList = Files.readAllBytes(Paths.get(args(0))).toList
  val bm = decode(byteList)
}
