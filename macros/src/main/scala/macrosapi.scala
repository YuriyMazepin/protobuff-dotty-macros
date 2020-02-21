package mazepin
package proto

import proto.api.{MessageCodec, Prepare, N}
import com.google.protobuf.{CodedOutputStream, CodedInputStream}
import scala.quoted._
import scala.quoted.matching._
import scala.internal.quoted.showName
import scala.collection.immutable.ArraySeq
import mazepin.proto.Bytes

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

  private[this] case class FieldInfo(name: String, num: Int, tpe: Type, getter: Symbol)

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
      val (name, tpe) = s.tree match
        case ValDef(vName,vTpt,vRhs) => (vName, vTpt.tpe)
        case _ => qctx.throwError(s"wrong param definition of case class `${typeName}`")

      FieldInfo(
        name = name
      , num = nums.collectFirst{ case (n, num) if n == name => num }.getOrElse(qctx.throwError(s"missing num for `${name}: ${typeName}`"))
      , tpe = tpe
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
          .orElse(writeOption(a, os, p))
      ).flatten.flatten
    , Expr.unitExpr)
  }

  private def writeBasic[A: Tpe](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): Option[List[Expr[Unit]]] =
    writeFun(os, field.tpe, getterTerm(a, field)).map(fun =>
      List(
        '{ ${os}.writeUInt32NoTag(${Expr(fieldTag(field))}) }
      , fun
      )
    )
  
  private def writeOption[A: Tpe](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): Option[List[Expr[Unit]]] =
    if field.tpe.isOption then
      val tpe = field.tpe.optionArgument
      val getter = getterTerm(a, field)
      val getterOption = getterOptionTerm(a, field)
      writeFun(os, tpe, getterOption).map(fun => {
        val isDefined = Select(getter, OptionClass.method("isDefined").head).seal.cast[Boolean]
        val expr = '{
          if ${isDefined} then {
            ${os}.writeUInt32NoTag(${Expr(fieldTag(field))})
            ${fun}
          }
        }
        List(expr)
      })
    else None

  private def writeFun(os: Expr[CodedOutputStream], t: Type, getterTerm: Term): Option[Expr[Unit]] = {
    val getValue = getterTerm.seal
    if t.isInt then Some('{ ${os}.writeInt32NoTag(${getValue.cast[Int]}) })
    else if t.isLong then Some('{ ${os}.writeInt64NoTag(${getValue.cast[Long]}) })
    else if t.isBoolean then Some('{ ${os}.writeBoolNoTag(${getValue.cast[Boolean]}) })
    else if t.isDouble then Some('{ ${os}.writeDoubleNoTag(${getValue.cast[Double]}) })
    else if t.isFloat then Some('{ ${os}.writeFloatNoTag(${getValue.cast[Float]}) })
    else if t.isString then Some('{ ${os}.writeStringNoTag(${getValue.cast[String]}) })
    else if t.isString then Some('{ ${os}.writeStringNoTag(${getValue.cast[String]}) })
    else if t.isArrayByte then Some('{ ${os}.writeByteArrayNoTag(${getValue.cast[Array[Byte]]}) })
    else if t.isArraySeqByte then Some('{ ${os}.writeByteArrayNoTag(${getValue.cast[ArraySeq[Byte]]}.toArray[Byte]) })
    else if t.isBytesType then Some('{ ${os}.writeByteArrayNoTag(${getValue.cast[Bytes]}.unsafeArray) })
    else None
  }

  private def sizeImpl[A: Tpe](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Int] = {
    val sizeSym = Symbol.newVal(ctx.owner, "sizeAcc", IntType, Flags.Mutable, Symbol.noSymbol)
    val sizeRef = Ref(sizeSym)
    val init = ValDef(sizeSym, Some(Literal(Constant(0))))
    val xs = params.map(p => {
      sizeBasic(a, p, sizeRef)
        .orElse(sizeOption(a, p, sizeRef))
    }).flatten
    Block(
      init +: xs
    , sizeRef
    ).seal.cast[Int]
  }

  private def sizeBasic[A: Tpe](a: Expr[A], field: FieldInfo, sizeAcc: Ref): Option[Statement] =
    sizeFun(field.tpe, getterTerm(a, field)).map(fun => {
      val sum = '{ ${sizeAcc.seal.cast[Int]} + CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
      Assign(sizeAcc, sum.unseal)
    })
  
  private def sizeOption[A: Tpe](a: Expr[A], field: FieldInfo, sizeAcc: Ref): Option[Statement] =
    if field.tpe.isOption then
      val tpe = field.tpe.optionArgument
      val getter = getterTerm(a, field)
      val getterOption = getterOptionTerm(a, field)
      sizeFun(tpe, getterOption).map(fun => {
        val sum = '{ ${sizeAcc.seal.cast[Int]} + CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
        val assign = Assign(sizeAcc, sum.unseal)
        val isDefined = Select(getter, OptionClass.method("isDefined").head)
        If(isDefined, assign, unitLiteral)
      })
    else None

  private def sizeFun(t: Type, getterTerm: Term): Option[Expr[Int]] = {
    val getValue = getterTerm.seal
    if t.isInt then Some('{ CodedOutputStream.computeInt32SizeNoTag(${getValue.cast[Int]}) })
    else if t.isLong then Some('{ CodedOutputStream.computeInt64SizeNoTag(${getValue.cast[Long]}) })
    else if t.isBoolean then Some('{ 1 })
    else if t.isDouble then Some('{ 8 })
    else if t.isFloat then Some('{ 4 })
    else if t.isString then Some('{ CodedOutputStream.computeStringSizeNoTag(${getValue.cast[String]}) })
    else if t.isArrayByte then Some('{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.cast[Array[Byte]]}) })
    else if t.isArraySeqByte then Some('{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.cast[ArraySeq[Byte]]}.toArray[Byte]) })
    else if t.isBytesType then Some('{ CodedOutputStream.computeByteArraySizeNoTag(${getValue.cast[Bytes]}.unsafeArray) })
    else None
  }

  private def getterTerm[A: Tpe](a: Expr[A], field: FieldInfo): Term =
    Select(a.unseal, field.getter)

  private def getterOptionTerm[A: Tpe](a: Expr[A], field: FieldInfo): Term =
    Select(Select(a.unseal, field.getter), OptionClass.method("get").head)

  private def OptionIsDefinedTerm(term: Term): Term = Select(term, OptionClass.method("isDefined").head)

  private def readImpl[A: Tpe](t: Tpe[A], params: List[FieldInfo], is: Expr[CodedInputStream])(using ctx: Context): Expr[A] = {

    if (params.size > 0) {
      val xs: List[(Statement, Term, Term)] = params.map(p => {
        val pType = p.tpe.seal.asInstanceOf[Tpe[Any]]
        val _none = '{ None:Option[${pType}] }.unseal
        val sym = Symbol.newVal(ctx.owner, s"${p.name}Read", _none.tpe.widen, Flags.Mutable, Symbol.noSymbol) 
        val init = ValDef(sym, Some(_none))
        val ref = Ref(sym).asInstanceOf[Ident]
        val res = resTerm(ref, p)
        (init, ref, res)
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
              val readFunExpr = readFun(p.tpe, is).get
              val assign = Assign(ref, readFunExpr.unseal)
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

  private def resTerm(ref: Ident, field: FieldInfo): Term =
    if (field.tpe.isOption) then ref
    else {
      val refExpr: Expr[Option[Any]] = ref.seal.cast[Option[Any]]
      val error = s"missing required field `${field.name}: ${field.tpe.typeSymbol.name}`"
      '{ ${refExpr}.getOrElse(throw new RuntimeException(${Expr(error)})) }.unseal
    }

  private def classApply[A: Tpe](t: Tpe[A], params: List[Term]): Term =
    t.unseal.tpe match
      case y: TermRef => Ident(y)
      case x @ TypeRef(_) =>
        val sym = x.typeSymbol
        val applyMethod = sym.companionModule.method("apply").head
        Apply(Select(Ident(TermRef(x.qualifier, sym.name)), applyMethod), params)

  private val ArrayByteType: Type = ('[Array[Byte]]).unseal.tpe
  private val ArraySeqByteType: Type = ('[ArraySeq[Byte]]).unseal.tpe
  private val BytesType: Type = ('[Bytes]).unseal.tpe
  private val NTpe: Type = ('[N]).unseal.tpe
  private def (t: Type) isNType: Boolean = t =:= NTpe
  private def (t: Type) isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
  private def caseClassParams[A: Tpe](t: Tpe[A]): List[Symbol] = t.unseal.tpe.typeSymbol.caseFields
  private def unitLiteral: Literal = Literal(Constant(()))

  private extension TypeOps on (t: Type) {
    def isString: Boolean = t =:= StringType
    def isInt: Boolean = t =:= IntType
    def isLong: Boolean = t =:= LongType
    def isBoolean: Boolean = t =:= BooleanType
    def isDouble: Boolean = t =:= DoubleType
    def isFloat: Boolean = t =:= FloatType
    def isArrayByte: Boolean = t =:= ArrayByteType
    def isArraySeqByte: Boolean = t =:= ArraySeqByteType
    def isBytesType: Boolean = t =:= BytesType
    def isOption: Boolean = t match {
      case AppliedType(t1, _) if t1.typeSymbol == OptionClass => true
      case _ => false
    }
    def optionArgument: Type = t match {
      case AppliedType(t1, args) if t1.typeSymbol == OptionClass => args.head.asInstanceOf[Type]
      case _ => qctx.throwError(s"It isn't Option type: ${t.typeSymbol.name}")
    }
  }

  private def readFun(t: Type, is: Expr[CodedInputStream]): Option[Expr[Some[Any]]] =
    if t.isInt then Some('{ Some(${is}.readInt32) })
    else if t.isLong then Some('{ Some(${is}.readInt64) })
    else if t.isBoolean then Some('{ Some(${is}.readBool) })
    else if t.isDouble then Some('{ Some(${is}.readDouble) })
    else if t.isFloat then Some('{ Some(${is}.readFloat) })
    else if t.isString then Some('{ Some(${is}.readString) })
    else if t.isArrayByte then Some('{ Some(${is}.readByteArray) })
    else if t.isArraySeqByte then Some('{ Some(ArraySeq.unsafeWrapArray(${is}.readByteArray)) })
    else if t.isBytesType then Some('{ Some(Bytes.unsafeWrap(${is}.readByteArray)) })
    else if t.isOption then readFun(t.optionArgument, is)
    else None

  private def fieldTag(field: FieldInfo): Int = field.num << 3 | wireType(field.tpe)

  private def wireType(t: Type): Int =
    if t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isOption then wireType(t.optionArgument)
    else if t.isString || t.isArrayByte || t.isArraySeqByte || t.isBytesType then 2
    else qctx.throwError(s"unsupported param type: ${t.typeSymbol.name}")

}