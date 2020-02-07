package mazepin
package proto

import proto.api.{MessageCodec, Prepare}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._

object macrosapi {
  inline def casecodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}

}

object Macro {
  def caseCodecAuto[A: Type](given qctx: QuoteContext): Expr[MessageCodec[A]] = Impl(qctx).caseCodecAuto[A]
}

class Impl(qctx: QuoteContext) extends Common(qctx) {
  def caseCodecAuto[A: Type]: Expr[MessageCodec[A]] = {
    val t = '[A]
    // println(t.unseal.tpe.classSymbol.map(_.caseFields.map(_.annots)))
    '{ new MessageCodec[A] {
        def read(is: CodedInputStream): A = ${ readImpl[A](t) }
       } 
     }
  }

  def readImpl[A: Type](t: Type[A]): Expr[A] = {
    classApply[A](t)
  }
}

trait Common(val qctx: QuoteContext) {
  import scala.quoted.{Type => Tpe}
  import qctx.tasty.{_, given}
  given ctx: QuoteContext = qctx

  def classApply[A: Tpe](t: Tpe[A]): Expr[A] = {
    t.unseal.tpe match {
      case x @ TypeRef(_) => 
        val sym = x.typeSymbol
        sym.companionModule.method("apply") match {
          case method :: _ => Apply(Select(Ident(TermRef(x.qualifier, sym.name)), method), Nil).seal.cast[A]
          case _ => ???
        }
      //TermRef(ThisType(TypeRef(NoPrefix,module class proto)),module Dog)
      case y: TermRef => Ident(y).seal.cast[A]
    }
  }
}