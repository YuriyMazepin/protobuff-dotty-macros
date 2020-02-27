package mazepin
package proto

import mazepin.proto.macrosapi.{casecodecAuto}
import mazepin.proto.api.{MessageCodec, N, decode, encode}

object Main {

  def main(args: Array[String]): Unit = {
    given codec2 as MessageCodec[Animal] = casecodecAuto[Animal]
    val a = Animal(name="Jerry", old=3, long=555, optionStr = Some("hello"), list = List("a", "b", "c"), intList = Set(1, 3, 5, 100))
    // val bytes = encode(a)
    val bytes: Array[Byte] = Array(10,5,74,101,114,114,121,-120,2,-85,4,16,3,34,5,104,101,108,108,111,82,1,97,82,1,98,82,1,99,-70,3,4,1,3,5,100)
    println(bytes.mkString(","))
    val res: Animal = decode(bytes)
    println(res)
  }
}

final case class Animal(
  @N(1) name: String
, @N(33) long: Long
, @N(2) old: Int
, @N(4) optionStr: Option[String]
, @N(10) list: List[String]
, @N(55) intList: Set[Int]
)