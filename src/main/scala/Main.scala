package mazepin
package proto

import mazepin.proto.macrosapi.casecodecAuto
import mazepin.proto.api.{MessageCodec, N, decode}

object Main {

  def main(args: Array[String]): Unit = {
    // implicit val codec1: MessageCodec[Test] = casecodecAuto[Test]
    implicit val codec1: MessageCodec[Dog.type] = casecodecAuto[Dog.type]
    val res: Dog.type = decode(Array())
    println(s"res ${res}")
  }

}

final case class Test() {
  def hello = "Hello"
}
final case class Animal(@N(1) name: String="n1", @N(2) color: String="c1", @N(3) old: Int=1)
case object Dog