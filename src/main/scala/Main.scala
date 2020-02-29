package mazepin
package proto

import mazepin.proto.macrosapi.{casecodecAuto}
import mazepin.proto.api.{MessageCodec, N, decode, encode, Prepare}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}

object Main {

  given codec3 as MessageCodec[Test] = casecodecAuto[Test]

  def main(args: Array[String]): Unit = {
    given codec2 as MessageCodec[Animal] = casecodecAuto[Animal]
    val a = Animal(name="Jerry", old=3, long=555, optionStr = Some("hello"), list = List("a", "b", "c"), intList = List(1, 3, 5, 100), test=Test("v1", 333))
    val bytes = encode(a)
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
, @N(55) intList: List[Int]
, @N(100) test: Test
)

final case class Test(@N(1) value1: String, @N(2) value2: Int)