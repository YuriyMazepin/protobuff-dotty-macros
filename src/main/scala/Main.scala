package mazepin
package proto

import mazepin.proto.macrosapi.casecodecAuto
import mazepin.proto.api.{MessageCodec, N, decode}

object Main {

  def main(args: Array[String]): Unit = {
    // given codec1: MessageCodec[Animal] = casecodecAuto[Animal]
    given codec2: MessageCodec[Dog.type] = casecodecAuto[Dog.type]
    // val res: Dog.type = decode(Array())
    // println(s"res ${res}")
  }
}

final case class Test() {
  def hello = "Hello"
}
final case class Animal(@N(1) name: String="Tom", @N(2) color: String="Black", @N(3) old: Int=1, @N(4) tpe: String = "cat")
case object Dog