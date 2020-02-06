package mazepin
package proto

import scala.quoted._

object TestMacro {
  inline def test[A]: A = ${ testImpl[A] }

  def testImpl[A: Type](given qctx: QuoteContext): Expr[A] = {
    import qctx.tasty.{_, given}
    val t = '[A]
    println(s"tpe: ${t.unseal.tpe}")
    t.unseal.tpe match {
      case t @ TypeRef(_) => 
        val sym = t.typeSymbol
        val applyMethod = sym.companionModule.method("apply")(0)
        val constrExpr: Expr[A] = Apply(Select(Ident(TermRef(t.qualifier, sym.name)), applyMethod), Nil).seal.cast[A]
        constrExpr
      case _ => ???
    }
  }
}