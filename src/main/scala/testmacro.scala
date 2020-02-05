package mazepin
package proto

import scala.quoted._

object TestMacro {
  inline def test: Animal = ${ testImpl }

  def testImpl(given qctx: QuoteContext): Expr[Animal] = {
    import qctx.tasty.{_, given}
    val r = '{ Animal() }
    // val r = '{ Animal(name="Tom",color="black",old=3) }
    println(s"creating cat ==> ${r.unseal}")
    r
  }
}