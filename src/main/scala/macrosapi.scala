package mazepin
package proto

import proto.api.{MessageCodec, Prepare, N}
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

  private[proto] case class FieldInfo(name: String, num: Int)

  def classApply[A: Tpe](t: Tpe[A]): Expr[A] = {

    val nums = caseClassParams[A](t).map(p => 
      p.annots.collect{ case Apply(Select(New(tpt),_), List(Literal(Constant(num: Int)))) if tpt.tpe.isNType => p.name -> num } match {
        case List(x) => x
        case Nil => qctx.error(s"missing ${NTypeSymbol.name} annotation for `${t.unseal.tpe.typeSymbol.name}`")
        case _ => qctx.error(s"multiple ${NTypeSymbol.name} annotations applied for `${t.unseal.tpe.typeSymbol.name}`")
      }
    )
    t.unseal.tpe match {
      case x @ TypeRef(_) =>
        val sym = x.typeSymbol
        sym.companionModule.method("apply") match {
          case method :: _ => Apply(Select(Ident(TermRef(x.qualifier, sym.name)), method), Nil).seal.cast[A]
          case _ => ???
        }
      case y: TermRef => Ident(y).seal.cast[A]
    }
  }

  val NTpe: Tpe[N] = '[N]
  val NTypeSymbol = NTpe.unseal.tpe.typeSymbol
  def (t: Type) isNType: Boolean = t.typeSymbol == NTypeSymbol
  def caseClassParams[A: Tpe](t: Tpe[A]): List[Symbol] = t.unseal.tpe.typeSymbol.caseFields
  
}