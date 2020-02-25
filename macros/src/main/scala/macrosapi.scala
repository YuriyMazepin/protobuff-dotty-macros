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
  import qctx.tasty.{_, given _}
  import qctx.tasty.defn._

  private[this] case class FieldInfo(name: String, num: Int, tpe: Type, tpt: TypeTree, getter: Symbol)

  def caseCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val t = summon[quoted.Type[A]]
    val aType = t.unseal.tpe
    val aTypeSymbol = aType.typeSymbol
    val typeName = t.unseal.tpe.typeSymbol.name
    val params: List[Symbol] = aTypeSymbol.caseFields
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
      , tpe = tpt.tpe
      , tpt = tpt
      , getter = aTypeSymbol.field(name)
      )
    }
    if (nums.exists(_._2 < 1)) qctx.throwError(s"nums ${nums} should be > 0")
    if (nums.size != fields.size) qctx.throwError(s"nums size ${nums} not equal to `${aType}` constructor params size ${fields.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) qctx.throwError(s"nums ${nums} should be unique")
    val codec = 
      '{ new MessageCodec[A] {
          def prepare(a: A): Prepare[A] = new Prepare[A](a) {
            val size: Int = ${ sizeImpl('a, fields) }
            def write(os: CodedOutputStream): Unit = ${ writeImpl('a, fields, 'os) }
          }
          def read(is: CodedInputStream): A = ${ readImpl(t.unseal.tpe, fields, 'is).cast[A] }
      }
    }
    // println(codec.show)
    codec
  }

  private def writeImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] ={
    Expr.block(
      params.map(p =>
        writeBasic(a, os, p)
          .orElse(writeOption(a, os, p))
      ).flatten.flatten
    , Expr.unitExpr)
  }

  private def writeBasic[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): Option[List[Expr[Unit]]] =
    writeFun(os, field.tpe, getterTerm(a, field)).map(fun =>
      List(
        '{ ${os}.writeUInt32NoTag(${Expr(fieldTag(field))}) }
      , fun
      )
    )
  
  private def writeOption[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): Option[List[Expr[Unit]]] =
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
    else if t.isArrayByte then Some('{ ${os}.writeByteArrayNoTag(${getValue.cast[Array[Byte]]}) })
    else if t.isArraySeqByte then Some('{ ${os}.writeByteArrayNoTag(${getValue.cast[ArraySeq[Byte]]}.toArray[Byte]) })
    else if t.isBytesType then Some('{ ${os}.writeByteArrayNoTag(${getValue.cast[Bytes]}.unsafeArray) })
    else None
  }

  private def sizeImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Int] = {
    val sizeSym = Symbol.newVal(ctx.owner, "sizeAcc", IntType, Flags.Mutable, Symbol.noSymbol)
    val sizeRef = Ref(sizeSym)
    val init = ValDef(sizeSym, Some(Literal(Constant(0))))
    val xs = params.map(p => {
      sizeBasic(a, p, sizeRef)
        .orElse(sizeOption(a, p, sizeRef))
        .orElse(sizeCollection(a, p, sizeRef))
    }).flatten
    Block(
      init +: xs
    , sizeRef
    ).seal.cast[Int]
  }

  private def sizeBasic[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): Option[Statement] =
    sizeFun(field.tpe, getterTerm(a, field)).map(fun => {
      val sum = '{ ${sizeAcc.seal.cast[Int]} + CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
      Assign(sizeAcc, sum.unseal)
    })
  
  private def sizeOption[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): Option[Statement] =
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

  private def sizeCollection[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref)(using ctx: Context): Option[Statement] = 
    if field.tpe.isIterable then {
      val tpe1 = field.tpe.iterableArgument
      // val collectionType = field.tpe.iterableBaseType
      val getter = getterTerm(a, field).seal.cast[Iterable[Any]]
      val pType = tpe1.seal.asInstanceOf[quoted.Type[Any]]
      val test = '{ ${getter}.foreach((v: ${pType}) => ${sizeFun(tpe1, 'v.unseal).get}) }
      // val test = '{ 
      //   for {
      //     y: ${pType} <- ${getter}
      //   } yield {
      //     val s: Int = ${sizeFun(tpe1, 'y.unseal).get}
      //     ()
      //   }
      // }
      println(test.show)
      None
    } else None

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

  private def getterTerm[A: quoted.Type](a: Expr[A], field: FieldInfo): Term =
    Select(a.unseal, field.getter)

  private def getterOptionTerm[A: quoted.Type](a: Expr[A], field: FieldInfo): Term =
    Select(Select(a.unseal, field.getter), OptionClass.method("get").head)

  private def readImpl(t: Type, params: List[FieldInfo], is: Expr[CodedInputStream])(using ctx: Context): Expr[Any] = {

    if (params.size > 0) {
      val xs: List[(Statement, Term, Term)] = params.map(p => {
        val (init, ref) = initValDef(p)
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
              val readContent = readFun(p.tpe, is).map(fun =>
                Assign(ref, '{Some(${fun})}.unseal).seal
              ).getOrElse{
                if p.tpe.isIterable then {
                  val tpe1 = p.tpe.iterableArgument
                  readFun(tpe1, is).map(fun => {
                    val addOneMethod = Select(ref, ref.tpe.termSymbol.method("addOne").head)
                    val addOneApply = Apply(addOneMethod, List(fun.unseal)).seal
                    if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then {
                      addOneApply
                    } else {
                      val readMessage = '{ while (${is}.getBytesUntilLimit > 0) ${addOneApply} }
                      putLimit(is, readMessage)
                    }
                  }).getOrElse{ ??? }
                } else ???
              }
              // ).getOrElse{
              //   if p.tpe.isIterable then ???
              //   else {
              //     val pType = p.tpe.seal.asInstanceOf[Tpe[Any]]
              //     val codecType = messageCodecFor(p.tpe).seal.asInstanceOf[Tpe[MessageCodec[Any]]]
              //     val codec = '{implicitly[${codecType}] }
              //     val readMsg = '{ Some(${codec}.read(${is})) }
              //     val assign = Assign(ref, readMsg.unseal)
              //     putLimit(is, assign.seal)
              //   }
              // }
              '{  if (tag == ${paramTag}) { 
                    tagMatch = true
                    ${readContent}
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
      , classApply(t, resTerms)
      ).seal
    } else {
      classApply(t, Nil).seal
    }
  }

  private def putLimit(is: Expr[CodedInputStream], read: Expr[Unit]): Expr[Unit] =
    '{
      val readSize: Int = ${is}.readRawVarint32
      val limit = ${is}.pushLimit(readSize)
      ${read}
      ${is}.popLimit(limit)
    }

  private def initValDef(field: FieldInfo)(using ctx: Context): (ValDef, Ident) =
    if field.tpe.isOption then {
      // val pType = field.tpe.seal.asInstanceOf[quoted.Type[Any]]
      // val _none = '{ None:${pType} }.unseal
      val _none = Typed(Ref(NoneModule), field.tpt)
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", field.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref
    } else if field.tpe.isIterable then {
      val tpe1 = field.tpe.iterableArgument
      val collectionType = field.tpe.iterableBaseType
      val collectionCompanion = collectionType.typeSymbol.companionModule
      val newBuilderMethod = collectionCompanion.method("newBuilder").head
      val builderType = appliedBuilderType(tpe1, field.tpe)
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", builderType, Flags.EmptyFlags, Symbol.noSymbol)
      val rhs = Select(Ref(collectionCompanion), newBuilderMethod)
      val init = ValDef(sym, Some(rhs))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref
    } else {
      val pType = field.tpe.seal.asInstanceOf[quoted.Type[Any]]
      val _none = '{ None:Option[${pType}] }.unseal
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", _none.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref
    }

  private def resTerm(ref: Ident, field: FieldInfo): Term =
    if field.tpe.isOption then ref
    else if field.tpe.isIterable then
      Select(ref, ref.tpe.termSymbol.method("result").head)
    else {
      val error = s"missing required field `${field.name}: ${field.tpe.typeSymbol.name}`"
      val exeption = '{ throw new RuntimeException(${Expr(error)}) }.unseal
      Apply(
        TypeApply(
          Select(ref, OptionClass.method("getOrElse").head)
        , List(field.tpt)
        )
      , List(exeption)
      )
    }

  private def classApply(t: Type, params: List[Term]): Term =
    t match
      case y: TermRef => Ident(y)
      case x @ TypeRef(_) =>
        val sym = x.typeSymbol
        val applyMethod = sym.companionModule.method("apply").head
        Apply(Select(Ref(sym.companionModule), applyMethod), params)

  private val ArrayByteType: Type = typeOf[Array[Byte]]
  private val ArraySeqByteType: Type = typeOf[ArraySeq[Byte]]
  private val BytesType: Type = typeOf[Bytes]
  private val NTpe: Type = typeOf[N]
  private val ItetableType: Type = typeOf[scala.collection.Iterable[Any]]
  // private val MessageCodecType: Type = typeOf[MessageCodec[Any]]
  private val CodedInputStreamType: Type = typeOf[CodedInputStream]
  private def (t: Type) isNType: Boolean = t =:= NTpe
  private def (t: Type) isCaseClass: Boolean = t.typeSymbol.flags.is(Flags.Case)
  private def (t: Type) isSealedTrait: Boolean = t.typeSymbol.flags.is(Flags.Sealed & Flags.Trait)
  private def (t: Type) isIterable: Boolean = t <:< ItetableType && !t.isArraySeqByte
  // private def messageCodecFor(t: Type): Type = AppliedType(MessageCodecType, List(t))
  private def unitLiteral: Literal = Literal(Constant(()))

  private def builderType: Type = typeOf[scala.collection.mutable.Builder[Unit, Unit]]
  private def appliedBuilderType(t1: Type, t2: Type): Type = builderType match {
    case AppliedType(tycon,_) => AppliedType(tycon, List(t1, t2))
    case _ => ???
  }

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
    def iterableArgument: Type = t match {
      case AppliedType(_, args) if t.isIterable => args.head.asInstanceOf[Type]
      case _ => qctx.throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")
    }
    def iterableBaseType: Type = t match {
      case AppliedType(t1, _) if t.isIterable => t1
      case _ => qctx.throwError(s"It isn't Iterable type: ${t.typeSymbol.name}")
    }
  }

  private def readFun(t: Type, is: Expr[CodedInputStream]): Option[Expr[Any]] =
    if t.isInt then Some('{ ${is}.readInt32 })
    else if t.isLong then Some('{ ${is}.readInt64 })
    else if t.isBoolean then Some('{ ${is}.readBool })
    else if t.isDouble then Some('{ ${is}.readDouble })
    else if t.isFloat then Some('{ ${is}.readFloat })
    else if t.isString then Some('{ ${is}.readString })
    else if t.isArrayByte then Some('{ ${is}.readByteArray })
    else if t.isArraySeqByte then Some('{ ArraySeq.unsafeWrapArray(${is}.readByteArray) })
    else if t.isBytesType then Some('{ Bytes.unsafeWrap(${is}.readByteArray) })
    else if t.isOption then readFun(t.optionArgument, is)
    else None

  private def fieldTag(field: FieldInfo): Int = field.num << 3 | wireType(field.tpe)

  private def wireType(t: Type): Int =
    if t.isInt || t.isLong || t.isBoolean then 0
    else if t.isDouble then 1
    else if t.isFloat then 5
    else if t.isOption then wireType(t.optionArgument)
    else if t.isString || 
            t.isArrayByte || 
            t.isArraySeqByte || 
            t.isBytesType || 
            t.isCaseClass ||
            t.isSealedTrait ||
            t.isIterable then 2
    else 2

}