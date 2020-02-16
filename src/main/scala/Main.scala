package mazepin
package proto

import mazepin.proto.macrosapi.{casecodecAuto}
import mazepin.proto.api.{MessageCodec, N, decode}

object Main {

  def main(args: Array[String]): Unit = {
    given codec1 as MessageCodec[Animal] = casecodecAuto[Animal]
    val bytes1 = Array[Byte](10,3,84,111,109,16,3)
    val res: Animal = decode(bytes1)
    println(res)
  }
}

final case class Test() {
  def hello = "Hello"
}
final case class Animal(@N(1) name: String, @N(2) old: Int)
case object Dog