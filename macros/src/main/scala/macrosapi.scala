package mazepin
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.quoted.matching._
import scala.internal.quoted.showName

object macrosapi {
  inline def casecodecAuto[A]: MessageCodec[A] = ${Macro.caseCodecAuto[A]}
}

object Macro {
  def caseCodecAuto[A: Type](using qctx: QuoteContext): Expr[MessageCodec[A]] = Impl().caseCodecAuto[A]
}

private class Impl(using qctx: QuoteContext) {
  import scala.quoted.{Type => Tpe}
  import qctx.tasty.{_, given _}
  import qctx.tasty.defn._

  private[this] case class FieldInfo(name: String, num: Int, tpt: TypeTree, getter: Symbol)

  def caseCodecAuto[A: Tpe]: Expr[MessageCodec[A]] = {
    val t = summon[Tpe[A]]
    val aType = t.unseal.tpe
    val aTypeSymbol = aType.typeSymbol
    val typeName = t.unseal.tpe.typeSymbol.name
    val params: List[Symbol] = caseClassParams[A](t)
    val nums: List[(String, Int)] = params.map(p => 
      p.annots.collect{ case Apply(Select(New(tpt),_), List(Literal(Constant(num: Int)))) if tpt.tpe.isNType => p.name -> num } match {
        case List(x) => x
        case Nil => qctx.throwError(s"missing ${NTpe.typeSymbol.name} annotation for `${typeName}`")
        case _ => qctx.throwError(s"multiple ${NTpe.typeSymbol.name} annotations applied for `${typeName}`")
      }
    )
    val fields: List[FieldInfo] = params.map{ s =>
      val (name, tpt) = s.tree match
        case ValDef(vName,vTpt,vRhs) => (vName, vTpt)
        case _ => qctx.throwError(s"wrong param definition of case class `${typeName}`")

      FieldInfo(
        name = name
      , num = nums.collectFirst{ case (n, num) if n == name => num }.getOrElse(qctx.throwError(s"missing num for `${name}: ${typeName}`"))
      , tpt = tpt
      , getter = aTypeSymbol.field(name)
      )
    }
    val codec = 
      '{ new MessageCodec[A] {
          def prepare(a: A): Prepare[A] = new Prepare[A](a) {
            val size: Int = ${ sizeImpl('a, fields) }
            def write(os: CodedOutputStream): Unit = ${ writeImpl('a, fields, 'os) }
          }
          def read(is: CodedInputStream): A = ${ readImpl(t, fields, 'is) }
      }
    }
    // println(codec.show)
    codec
  }

  private def writeImpl[A: Tpe](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] ={
    Expr.block(
      params.map(p =>
        writeBasic(a, os, p)
      ).flatten.flatten
    , Expr.unitExpr)
  }

  private def writeBasic[A: Tpe](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): Option[List[Expr[Unit]]] =
    writeFun(a, os, field).map(fun =>
      List(
        '{ ${os}.writeUInt32NoTag(${Expr(fieldTag(field))}) }
      , fun
      )
    )

  private def writeFun[A: Tpe](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): Option[Expr[Unit]] = {
    val getValue = Select(a.unseal, field.getter).seal
    val t = field.tpt.tpe
    if t.isInt then Some('{ ${os}.writeInt32NoTag(${getValue.cast[Int]}) })
    else if t.isLong then Some('{ ${os}.writeInt64NoTag(${getValue.cast[Long]}) })
    else if t.isBoolean then Some('{ ${os}.writeBoolNoTag(${getValue.cast[Boolean]}) })
    else if t.isDouble then Some('{ ${os}.writeDoubleNoTag(${getValue.cast[Double]}) })
    else if t.isFloat then Some('{ ${os}.writeFloatNoTag(${getValue.cast[Float]}) })
    else if t.isString then Some('{ ${os}.writeStringNoTag(${getValue.cast[String]}) })
    else None
  }

  private def sizeImpl[A: Tpe](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Int] = {
    val sizeSym = Symbol.newVal(ctx.owner, "sizeAcc", IntType, Flags.Mutable, Symbol.noSymbol)
    val sizeRef = Ref(sizeSym)
    val sizeExpr = sizeRef.seal.cast[Int]
    val init = ValDef(sizeSym, Some(Literal(Constant(0))))
    val xs = params.map(p => {
      sizeBasic(a, p).map(exp => {
        val sum: Expr[Int] = '{ ${sizeExpr} + ${exp} }
        Assign(sizeRef, sum.unseal)
      })
    }).flatten
    Block(
      init +: xs
    , sizeRef
    ).seal.cast[Int]
  }

  private def sizeBasic[A: Tpe](a: Expr[A], field: FieldInfo): Option[Expr[Int]] =
    sizeFun(a, field).map(fun => {
      '{ CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
    })

  private def sizeFun[A: Tpe](a: Expr[A], field: FieldInfo): Option[Expr[Int]] = {
    val getValue = Select(a.unseal, field.getter).seal
    val t = field.tpt.tpe
    if t.isInt then Some('{ CodedOutputStream.computeInt32SizeNoTag(${getValue.cast[Int]}) })
    else if t.isLong then Some('{ CodedOutputStream.computeInt64SizeNoTag(${getValue.cast[Long]}) })
    else if t.isBoolean then Some('{ 1 })
    else if t.isDouble then Some('{ 8 })
    else if t.isFloat then Some('{ 4 })
    else if t.isString then Some('{ CodedOutputStream.computeStringSizeNoTag(${getValue.cast[String]}) })
    else None
  }

  private def readImpl[A: Tpe](t: Tpe[A], params: List[FieldInfo], is: Expr[CodedInputStream])(using ctx: Context): Expr[A] = {

    if (params.size > 0) {
      val xs: List[(Statement, Term, Term)] = params.map(p => {
        val pType = p.tpt.tpe.seal.asInstanceOf[Tpe[Any]]
        val _none = '{ None:Option[${pType}] }.unseal
        val sym = Symbol.newVal(ctx.owner, s"${p.name}Read", _none.tpe.widen, Flags.Mutable, Symbol.noSymbol) 
        val init = ValDef(sym, Some(_none))
        val ref = Ref(sym).asInstanceOf[Ident]
        val refExpr: Expr[Option[Any]] = ref.seal.cast[Option[Any]]
        val nameExpr = Expr(p.name)
        val numExpr = Expr(p.num)
        val resTerm = '{ ${refExpr}.getOrElse(throw new RuntimeException("missign required field")) }.unseal //todo show field in exeption
        (init, ref, resTerm)
      })

      val read = '{
        var done = false;
        while (done == false) {
          val tag: Int = ${is}.readTag
          var tagMatch: Boolean = false
          if (tag == 0) { done = true; tagMatch = true }
          ${
            Expr.block(params.zip(xs).map{ case (p, (_,ref,_)) => {
              val paramTag = Expr(fieldTag(p))
              val assign = Assign(ref, readFun(p, is).unseal)
              '{  if (tag == ${paramTag}) { 
                    tagMatch = true
                    ${assign.seal}
                  }
              }
            }}, Expr.unitExpr)
          }
          if (tagMatch == false) ${is}.skipField(tag)
        }
      }

      val statements = xs.map(_._1) :+ (read.unseal)
      val resTerms = xs.map(_._3)
      Block(
        statements
      , classApply[A](t, resTerms)
      ).seal.cast[A]
    } else {
      classApply[A](t, Nil).seal.cast[A]
    }
  }

  private def classApply[A: Tpe](t: Tpe[A], params: List[Term]): Term =
    t.unseal.tpe match
      case y: TermRef => Ident(y)
      case x @ TypeRef(_) =>
        val sym = x.typeSymbol
        val applyMethod = sym.companionModule.method("apply").head
        Apply(Select(Ident(TermRef(x.qualifier, sym.name)), applyMethod), params)

  private val NTpe: Type = ('[N]).unseal.tpe
  private def (t: Type) isNType: Boolean = t =:= NTpe
  private def (t: Type) isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
  private def caseClassParams[A: Tpe](t: Tpe[A]): List[Symbol] = t.unseal.tpe.typeSymbol.caseFields

  private extension TypeOps on (t: Type) {
    def isString: Boolean = t =:= StringType
    def isInt: Boolean = t =:= IntType
    def isLong: Boolean = t =:= LongType
    def isBoolean: Boolean = t =:= BooleanType
    def isDouble: Boolean = t =:= DoubleType
    def isFloat: Boolean = t =:= FloatType
  }

  private def readFun(field: FieldInfo, is: Expr[CodedInputStream]): Expr[Some[Any]] =
    val t: Type = field.tpt.tpe
    if t.isInt then '{ Some(${is}.readInt32) }
    else if t.isLong then '{ Some(${is}.readInt64) }
    else if t.isBoolean then '{ Some(${is}.readBool) }
    else if t.isDouble then '{ Some(${is}.readDouble) }
    else if t.isFloat then '{ Some(${is}.readFloat) }
    else if t.isString then '{ Some(${is}.readString) }
    else ???

  private def fieldTag(field: FieldInfo): Int = field.num << 3 | wireType(field.tpt.tpe)

  private def wireType(t: Type): Int =
    if t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isString then 2
    else qctx.throwError(s"unsupported param type: ${t.typeSymbol.name}")

}