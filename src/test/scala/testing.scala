package mazepin.proto.test

import org.junit.Test
import org.junit.Assert._
import java.util.Arrays

import mazepin.proto.macrosapi.casecodecAuto
import mazepin.proto.api.{MessageCodec, N, decode, encode, Prepare}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}

object models {
  case class Basic(
    @N(21) int: Int
  , @N(22) long: Long
  , @N(23) bool: Boolean
  , @N(24) double: Double
  , @N(25) float: Float
  , @N(26) str: String
  , @N(50) bytes: Array[Byte]
  )
}

class Testing {
  import models._
  
  @Test def encodeDecode(): Unit = {
    implicit val codec: MessageCodec[Basic] = casecodecAuto[Basic]
    basic
  }

  def basic(using codec: MessageCodec[Basic]): Unit = {
    (for {
      int <- List(Int.MinValue, -2, -1, 0, 1, 2, Int.MaxValue)
      long <- List(Long.MinValue, -2L, -1L, 0L, 1L, 2L, Long.MaxValue)
      bool <- List(false, true)
      double <- List(Double.MinValue, -2.0D, -1.0D, 0.0D, 1.0D, 2.0D, Double.MaxValue)
      float <- List(Float.MinValue, -2.0F, -1.0F, 0.0F, 1.0F, 2.0F, Float.MaxValue)
      str <- List("", "str")
      bytes <- List(Array.empty[Byte], Array(0.toByte), Array(1.toByte), Array(2.toByte), Array(255.toByte))
    } yield Basic(int = int, long = long, bool = bool, double = double, float = float, str = str, bytes = bytes)).foreach{ data =>
      val decoded = decode(encode(data))
      assert(decoded.int == data.int)
      assert(decoded.long == data.long)
      assert(decoded.bool == data.bool)
      assert(decoded.double == data.double)
      assert(decoded.float == data.float)
      assert(decoded.str == data.str)
      assert(Arrays.equals(decoded.bytes, data.bytes))
    }
  }
}