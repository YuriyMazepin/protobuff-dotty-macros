package mazepin
package proto

import proto.api.{MessageCodec, Prepare}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._

object macrosapi {
  inline def casecodecAuto[A]: MessageCodec[A] = ${Impl.caseCodecAuto[A]}

}

object Impl {
  def caseCodecAuto[A: Type](given QuoteContext): Expr[MessageCodec[A]] = '{
    new MessageCodec[A] {
      def prepare(a: A): Prepare = ???
      def read(is: CodedInputStream): A = ${readImpl[A]}
    }
  }

  def readImpl[A: Type]: Expr[A] = {
    val tpe: Type[A] = summon[Type[A]]
    ???
  }

}