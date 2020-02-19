package mazepin
package proto

import mazepin.proto.macrosapi.{casecodecAuto}
import mazepin.proto.api.{MessageCodec, N, decode, encode}

object Main {

  def main(args: Array[String]): Unit = {
    given codec1 as MessageCodec[Animal] = casecodecAuto[Animal]
    val a = Animal(name="Jerry", old=3, long=555)
    val bytes = encode(a)
    println(bytes.mkString(","))
    val res: Animal = decode(bytes)
    println(res)
  }
}

final case class Test() {
  def hello = "Hello"
}
final case class Animal(@N(1) name: String, @N(33) long: Long, @N(2) old: Int)
case object Dog
