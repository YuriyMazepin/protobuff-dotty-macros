package mazepin
package proto

import mazepin.proto.macrosapi.casecodecAuto
import mazepin.proto.api.{MessageCodec, N, decode}

object Main {

  def main(args: Array[String]): Unit = {
    // implicit val codec1: MessageCodec[Animal] = casecodecAuto[Animal]
    // implicit val codec1: MessageCodec[Test] = casecodecAuto[Test]
    // val codec2: MessageCodec[Dog.type] = casecodecAuto[Dog.type]

    // val res: Animal = decode(Array())
    val res = TestMacro.test[Test]
    println(s"res => ${res.hello}")
  }

}

final case class Test() {
  def hello = "Hello"
}
final case class Animal(@N(1) name: String="n1", @N(2) color: String="c1", @N(3) old: Int=1)
case object Dog