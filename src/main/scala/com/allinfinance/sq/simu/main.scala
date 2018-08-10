package com.allinfinance.sq.simu

import com.allinfinance.xsd._

import akka.actor._
import java.net.InetSocketAddress
import akka.util.{ByteString, ByteStringBuilder}

class SQServer(port: Int) extends Actor {
    val state = IO.IterateeRef.Map.async[IO.Handle]()(context.dispatcher)

    override def preStart {
        IOManager(context.system).listen(new InetSocketAddress(port))
    }

    def receive = {
        case IO.NewClient(server) =>
            val socket = server.accept()
            state(socket) flatMap {_ => SQServer.processRequest(socket)}

        case IO.Read(socket, bytes) =>
            state(socket)(IO Chunk bytes)

        case IO.Closed(socket, cause) =>
            state(socket)(IO EOF None)
            state -= socket
    }
}

object SQServer {
    def processRequest(socket: IO.SocketHandle): IO.Iteratee[Unit] =
        IO repeat {
            for {
                request <- readRequest
            } yield {
                socket write Response.bytes("hello").compact
                socket.close()
            }
        }

    def ascii(bytes: ByteString): String = bytes.decodeString("GBK")

    def readRequest = 
        for {
            len <- readLength
            msg <- IO take len
        } yield msg

    def readLength =
        for {
            len <- IO take 6
        } yield ascii(len).toInt
}

object Response {
    def bytes(msg: String) = {
        new ByteStringBuilder ++=
            ByteString("%06d".format(msg.length)) ++=
            ByteString(msg.getBytes("GBK")) result
    }
}

object SqSimu {
    def main(args: Array[String]): Unit = {
        val port = 6789
        ActorSystem().actorOf(Props(new SQServer(port)))
    }
}

