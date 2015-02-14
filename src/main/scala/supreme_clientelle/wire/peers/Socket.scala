package supreme_clientelle.wire.peers

import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{CompletionHandler, AsynchronousSocketChannel, AsynchronousServerSocketChannel}

import scala.concurrent.{Promise, Future}

/**
 * Author: @aguestuser
 * Date: 2/12/15
 * License: GPLv2
 */

object Socket {

    type SSC = AsynchronousServerSocketChannel
    type SC = AsynchronousSocketChannel
    type SAddr = InetSocketAddress

    def getServerSock(port: Int): SSC = {
      val sc = AsynchronousServerSocketChannel.open().bind(new InetSocketAddress(port))
      println(s"Listening on port $port")
      sc
    }

    def getClientSock(port: Int): SC =
      AsynchronousSocketChannel.open().bind(new InetSocketAddress(port))

    def accept(sSock: SSC): Future[SC] = {
      val p = Promise[SC]()
      sSock.accept(null, new CompletionHandler[SC, Void] {
        def completed(client: SC, att: Void) = {
          println(s"Client connection received from ${client.getRemoteAddress}")
          p success { client }
        }
        def failed(e: Throwable, att: Void) = p failure { e }
      })
      p.future
    }

    def connect(sock: SC, addr: InetSocketAddress): Future[Unit] = {
      val p = Promise[Unit]()
      sock.connect(addr, null, new CompletionHandler[Void,Void] {
        def completed(done: Void, att: Void) = {
          println(s"Connected to remote socket at $addr")
          p success { () }
        }
        def failed(e: Throwable, att: Void) = p failure { e }
      })
      p.future
    }

    def read(cSock: SC): Future[Array[Byte]] = {
      val buf = ByteBuffer.allocate(1024)
      val p = Promise[Array[Byte]]()
      cSock.read(buf, null, new CompletionHandler[Integer, Void] {
        def completed(numRead: Integer, att: Void) = {
          //println(s"Read $numRead bytes") TODO add verbose mode?
          buf.flip()
          p success { buf.array() }
        }
        def failed(e: Throwable, att: Void) = p failure { e }
      })
      p.future
    }

    def write(sock: SC, msg: Array[Byte]): Future[Unit] =
      writeOnce(sock, msg) flatMap { numwrit =>
        if (numwrit == msg.size) Future.successful(())
        else write(sock, msg.drop(numwrit))
      }
    // TODO replace drop with slices of the array? (drop will run in O(n^2) where n is bytes being dropped)

    private def writeOnce(sock: SC, bs: Array[Byte]): Future[Integer] = {
      val p = Promise[Integer]()
      sock.write(ByteBuffer.wrap(bs), null, new CompletionHandler[Integer, Void] {
        def completed(numwrit: Integer, att: Void) = { /*println(s"Wrote $numwrit bytes");*/ p success { numwrit } }
        def failed(e: Throwable, att: Void) = p failure { e }
      })
      p.future
    }

    def strFromWire(msg: Array[Byte]): String = trimByteArray(msg).map(_.toChar).mkString.trim
    def trimByteArray(msg: Array[Byte]): Array[Byte] = msg.takeWhile(!Set(10:Byte, 13:Byte, 0:Byte).contains(_))

}
