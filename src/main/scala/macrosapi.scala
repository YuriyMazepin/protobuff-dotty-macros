package mazepin
package proto

import proto.api.{MessageCodec, Prepare}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._

object macrosapi {
  inline def casecodecAuto[A]: MessageCodec[A] = ${Impl.caseCodecAuto[A]}

}

object Impl {
  def caseCodecAuto[A: Type](given qctx: QuoteContext): Expr[MessageCodec[A]] = {
    import qctx.tasty.{_, given}
    val t = '[A]
    // println(t.unseal.tpe.classSymbol.map(_.caseFields.map(_.annots)))
    '{ new MessageCodec[A] {
        def read(is: CodedInputStream): A = ???//${ applyCaseClass[A]("Test") }
       } 
     }
  }

  // def applyCaseClass[A](name: String)(given qctx: QuoteContext): Expr[A] = {
  //   import qctx.tasty.{_, given}
  //   Apply(Select(Ident(name),"apply"),List())
  // }
}