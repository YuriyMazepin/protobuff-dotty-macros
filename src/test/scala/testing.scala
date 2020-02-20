package mazepin.proto.test

import org.junit.Test
import org.junit.Assert._
import java.util.Arrays

import mazepin.proto.macrosapi.casecodecAuto
import mazepin.proto.api.{MessageCodec, N, decode, encode, Prepare}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.collection.immutable.ArraySeq
import mazepin.proto.Bytes

object models {
  final case class Basic(
    @N(21) int: Int
  , @N(22) long: Long
  , @N(23) bool: Boolean
  , @N(24) double: Double
  , @N(25) float: Float
  , @N(26) str: String
  , @N(50) bytes: Array[Byte]
  )

  final case class ClassWithArray(@N(1) x: Array[Byte])
  final case class ClassWithArraySeq(@N(1) y: ArraySeq[Byte])
  final case class ClassWithBytes(@N(1) z: Bytes)

  given MessageCodec[Basic] = casecodecAuto
  given MessageCodec[ClassWithArray] = casecodecAuto
  given MessageCodec[ClassWithArraySeq] = casecodecAuto
  given MessageCodec[ClassWithBytes] = casecodecAuto
}

class Testing {
  import models._
  
  @Test def encodeDecode(): Unit = {
    // given MessageCodec[Basic] = casecodecAuto
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

  //ArraySeq[Byte] is compatible with Array[Byte]
  @Test def test1(): Unit = {
    import java.util.Arrays
    assert(Arrays.equals(decode[ClassWithArraySeq](encode[ClassWithArray](ClassWithArray(x=Array[Byte](1,2,3)))).y.unsafeArray.asInstanceOf[Array[Byte]], Array[Byte](1,2,3)))
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithArraySeq](ClassWithArraySeq(y=ArraySeq.unsafeWrapArray[Byte](Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
  }

  //"Bytes is compatible with Array[Byte]"
  @Test def test2(): Unit = {
    import java.util.Arrays
    assert(Arrays.equals(decode[ClassWithBytes](encode[ClassWithArray](ClassWithArray(x=Array[Byte](1,2,3)))).z.unsafeArray, Array[Byte](1,2,3)))
    assert(Arrays.equals(decode[ClassWithArray](encode[ClassWithBytes](ClassWithBytes(z=Bytes.unsafeWrap(Array[Byte](1,2,3))))).x, Array[Byte](1,2,3)))
  }

  //array byte wrapper encode
  @Test def test3(): Unit = {
    val data = ClassWithArray(Array(6, 7, 8, 9, 0))
    val encoded: Array[Byte] = encode(data)
    assert(Arrays.equals(Array[Byte](10,5, 6,7,8,9,0), encoded))
  }
}