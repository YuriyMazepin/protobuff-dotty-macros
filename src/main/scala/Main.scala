package mazepin
package proto

import mazepin.proto.macrosapi.{casecodecAuto}
import mazepin.proto.api.{MessageCodec, N, decode, encode}

object Main {

  def main(args: Array[String]): Unit = {
    given codec1 as MessageCodec[Embeded] = casecodecAuto[Embeded]
    given codec2 as MessageCodec[Animal] = casecodecAuto[Animal]
    val a = Animal(name="Jerry", old=3, long=555, optionStr = Some("hello"))//, xyz = Embeded(x=3,y="msg",z=true))
    val bytes = encode(a)
    println(bytes.mkString(","))
    val res: Animal = decode(bytes)
    println(res)
  }
}

final case class Test() {
  def hello = "Hello"
}
final case class Animal(
  @N(1) name: String
, @N(33) long: Long
, @N(2) old: Int
, @N(4) optionStr: Option[String]
// , @N(5) xyz: Embeded
)
final case class Embeded(@N(10) x: Int, @N(11) y: String, @N(12) z: Boolean)
