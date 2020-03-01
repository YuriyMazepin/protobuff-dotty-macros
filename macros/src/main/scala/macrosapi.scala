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

private class Impl(using val qctx: QuoteContext) extends Common {
  import qctx.tasty.{_, given _}
  import qctx.tasty.defn._

  def caseCodecAuto[A: quoted.Type]: Expr[MessageCodec[A]] = {
    val ctx = summon[Context]
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
      , sizeSym = Symbol.newVal(ctx.owner, s"${name}Size", IntType, Flags.Mutable, Symbol.noSymbol)
      , prepareSym = Symbol.newVal(ctx.owner, s"${name}Prepare", PrepareType, Flags.Mutable, Symbol.noSymbol)
      , prepareOptionSym = Symbol.newVal(ctx.owner, s"${name}Prepare", appliedOptionType(PrepareType), Flags.Mutable, Symbol.noSymbol)
      , prepareArraySym = Symbol.newVal(ctx.owner, s"${name}Prepare", typeOf[Array[Prepare]], Flags.Mutable, Symbol.noSymbol)
      )
    }
    if (nums.exists(_._2 < 1)) qctx.throwError(s"nums ${nums} should be > 0")
    if (nums.size != fields.size) qctx.throwError(s"nums size ${nums} not equal to `${aType}` constructor params size ${fields.size}")
    if (nums.groupBy(_._2).exists(_._2.size != 1)) qctx.throwError(s"nums ${nums} should be unique")

    val codec = '{ 
      new MessageCodec[A] {
        def prepare(a: A): Prepare = ${ prepareImpl('a, fields) }
        def read(is: CodedInputStream): A = ${ readImpl(t.unseal.tpe, fields, 'is).cast[A] }
      }
    }
    // println(codec.show)
    codec
  }

  private def prepareImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo])(using ctx: Context): Expr[Prepare] =
    val sizeAccSym = Symbol.newVal(ctx.owner, "sizeAcc", IntType, Flags.Mutable, Symbol.noSymbol)
    val sizeAccRef = Ref(sizeAccSym)
    val sizeAccValDef = ValDef(sizeAccSym, Some(Literal(Constant(0))))
    val xs = params.flatMap(p => size(a, p, sizeAccRef))
    val newPrepare = '{
      new Prepare {
        val size: Int = ${ sizeAccRef.seal.cast[Int] }
        def write(os: CodedOutputStream): Unit = ${ writeImpl(a, params, 'os) }
      }
    }.unseal
    Block(
      sizeAccValDef :: xs
    , newPrepare
    ).seal.cast[Prepare]

  private def writeImpl[A: quoted.Type](a: Expr[A], params: List[FieldInfo], os: Expr[CodedOutputStream]): Expr[Unit] =
    Expr.block(
      params.flatMap(p =>
        if p.tpe.isCommonType then writeCommon(a, os, p)
        else if p.tpe.isOption then writeOption(a, os, p)
        else if p.tpe.isIterable then writeCollection(a, os, p)
        else writeMessage(a, os, p)
      )
    , Expr.unitExpr)

  private def writeCommon[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , writeFun(os, field.tpe, getterTerm(a, field))
    )
  
  private def writeOption[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe = field.tpe.optionArgument
    val getter = getterTerm(a, field)
    val getterOption = getterOptionTerm(a, field)
    if tpe.isCommonType then
      List(
        '{
          if ${getter.seal.cast[Option[Any]]}.isDefined then {
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${writeFun(os, tpe, getterOption)}
          }
        }
      )
    else
      val prepareOptionRef = Ref(field.prepareOptionSym).seal.cast[Option[Prepare]]
      List(
        '{
          if ${prepareOptionRef}.isDefined then {
            val p = ${prepareOptionRef}.get
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os})
          }
        }
      )

  private def writeCollection[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val tpe1 = field.tpe.iterableArgument
    val getter = getterTerm(a, field).seal.cast[Iterable[Any]]
    val pType = tpe1.seal.asInstanceOf[quoted.Type[Any]]
    val sizeRef = Ref(field.sizeSym)
    if tpe1.isCommonType then
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
        val expr = '{
          ${getter}.foreach((v: ${pType}) => {
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${writeFun(os, tpe1, 'v.unseal)}
          })
        }
        List(expr)
      else
        List(
          '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
        , '{ ${os}.writeUInt32NoTag(${sizeRef.seal.cast[Int]}) }
        , '{ ${getter}.foreach((v: ${pType}) => ${writeFun(os, tpe1, 'v.unseal)} ) }
        )
    else
      val prepareArrayRef = Ref(field.prepareArraySym).seal.cast[Array[Prepare]]
      val counterName = s"${field.name}Counter"
      List(
        '{
          @showName(${Expr(counterName)})
          var counter = 0
          while (counter < ${prepareArrayRef}.length) {
            val p = ${prepareArrayRef}(counter)
            ${os}.writeUInt32NoTag(${Expr(field.tag)})
            ${os}.writeUInt32NoTag(p.size)
            p.write(${os}) 
            counter = counter + 1
          }
        }
      )

  private def writeMessage[A: quoted.Type](a: Expr[A], os: Expr[CodedOutputStream], field: FieldInfo): List[Expr[Unit]] =
    val prepareRef = Ref(field.prepareSym).seal.cast[Prepare]
    List(
      '{ ${os}.writeUInt32NoTag(${Expr(field.tag)}) }
    , '{ ${os}.writeUInt32NoTag(${prepareRef}.size) }
    , '{ ${prepareRef}.write(${os}) }
    )

  private def size[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
   if field.tpe.isCommonType then sizeCommon(a, field, sizeAcc)
   else if field.tpe.isOption then sizeOption(a, field, sizeAcc)
   else if field.tpe.isIterable then sizeCollection(a, field, sizeAcc)
   else sizeMessage(a, field, sizeAcc)

  private def sizeCommon[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val fun = sizeFun(field.tpe, getterTerm(a, field))
    val sum = '{ CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
    List(increment(sizeAcc, sum))
  
  private def sizeOption[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val tpe = field.tpe.optionArgument
    val getter = getterTerm(a, field)
    val getterOption = getterOptionTerm(a, field)
    if (tpe.isCommonType) then
      val fun = sizeFun(tpe, getterOption)
      val sum = '{ CodedOutputStream.computeTagSize(${Expr(field.num)}) + ${fun} }
      val incrementSize = increment(sizeAcc, sum)
      val isDefined = Select(getter, OptionClass.method("isDefined").head)
      List(If(isDefined, incrementSize, unitLiteral))
    else
      val prepareOptionRhs = '{
        if (${getter.seal.cast[Option[Any]]}.isDefined) {
          val p: Prepare = ${findCodec(tpe)}.prepare(${getterOption.seal})
          ${
            increment(
              sizeAcc
            , '{CodedOutputStream.computeTagSize(${Expr(field.num)}) + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).seal
          }
          Some(p)
        } else None
      }
      List(
        ValDef(field.prepareOptionSym, Some(prepareOptionRhs.unseal))
      )

  private def sizeCollection[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] = 
    val tpe1 = field.tpe.iterableArgument
    val getter = getterTerm(a, field).seal.cast[Iterable[Any]]
    val pType = tpe1.seal.asInstanceOf[quoted.Type[Any]]
    if tpe1.isCommonType then
      val sizeRef = Ref(field.sizeSym)
      val sizeValDef = ValDef(field.sizeSym, Some(Literal(Constant(0))))
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then
        val tagSizeName = s"${field.name}TagSize"
        val sizeExpr = '{ 
          @showName(${Expr(tagSizeName)})
          val tagSize = CodedOutputStream.computeTagSize(${Expr(field.num)})
          ${getter}.foreach((v: ${pType}) => ${ increment(sizeRef, '{ ${sizeFun(tpe1, 'v.unseal)} + tagSize }).seal }  )
        }
        val incrementAcc = increment(sizeAcc, sizeRef.seal.cast[Int])
        List(sizeValDef, sizeExpr.unseal, incrementAcc)
      else
        val sizeExpr = '{
          ${getter}.foreach((v: ${pType}) => ${ increment(sizeRef, sizeFun(tpe1, 'v.unseal)).seal } )
        }
        val sizeRefExpr = sizeRef.seal.cast[Int]
        val sum = '{ 
          CodedOutputStream.computeTagSize(${Expr(field.num)}) + 
          CodedOutputStream.computeUInt32SizeNoTag(${sizeRefExpr}) +
          ${sizeRefExpr}
        }
        val incrementAcc = increment(sizeAcc, sum)
        List(sizeValDef, sizeExpr.unseal, incrementAcc)
    else
      val prepareArrayRef = Ref(field.prepareArraySym)
      val prepareArrayRhs = '{ new Array[Prepare](${getter}.size) }
      val counterName = s"${field.name}Counter"
      val sizeExpr = '{
        @showName(${Expr(counterName)})
        var counter = 0
        ${getter}.foreach((v: ${pType}) => {
          val p: Prepare = ${findCodec(tpe1)}.prepare(v)
          ${prepareArrayRef.seal.cast[Array[Prepare]]}(counter) = p
          ${
            increment(
              sizeAcc
            , '{CodedOutputStream.computeTagSize(${Expr(field.num)}) + CodedOutputStream.computeUInt32SizeNoTag(p.size) + p.size }
            ).seal
          }
          counter = counter + 1
        })
      }
      List(
        ValDef(field.prepareArraySym, Some(prepareArrayRhs.unseal))
      , sizeExpr.unseal
      )

  private def sizeMessage[A: quoted.Type](a: Expr[A], field: FieldInfo, sizeAcc: Ref): List[Statement] =
    val getter = getterTerm(a, field).seal
    val prepare = '{ ${findCodec(field.tpe)}.prepare(${getter}) }
    val prepareValDef = ValDef(field.prepareSym, Some(prepare.unseal))
    val prepareRef = Ref(field.prepareSym).seal.cast[Prepare]
    val sum = '{ 
      CodedOutputStream.computeTagSize(${Expr(field.num)}) + 
      CodedOutputStream.computeUInt32SizeNoTag(${prepareRef}.size) +
      ${prepareRef}.size
    }
    val incrementAcc = increment(sizeAcc, sum)
    List(prepareValDef, incrementAcc)

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
              val paramTag = Expr(p.tag)
              val readContent = readContentImpl(p, ref, is)
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

  private def readContentImpl(p: FieldInfo, readRef: Term, is: Expr[CodedInputStream]): Expr[Any] =
    if (p.tpe.isCommonType) then
      val fun = readFun(p.tpe, is)
      Assign(readRef, '{Some(${fun})}.unseal).seal
    else if p.tpe.isOption && p.tpe.optionArgument.isCommonType then
      val fun = readFun(p.tpe.optionArgument, is)
      Assign(readRef, '{Some(${fun})}.unseal).seal
    else if p.tpe.isOption then
      putLimit(
        is
      , Assign(readRef, '{Some(${findCodec(p.tpe.optionArgument)}.read(${is}))}.unseal).seal 
      )
    else if p.tpe.isIterable && p.tpe.iterableArgument.isCommonType then
      val tpe1 = p.tpe.iterableArgument
      val fun = readFun(tpe1, is)
      val addOneApply = Apply(
        Select(readRef, readRef.tpe.termSymbol.method("addOne").head)
      , List(fun.unseal)
      ).seal
      if tpe1.isString || tpe1.isArrayByte || tpe1.isArraySeqByte || tpe1.isBytesType then 
        addOneApply
      else
        putLimit(
          is
        , '{ while (${is}.getBytesUntilLimit > 0) ${addOneApply} }
        )
    else if p.tpe.isIterable then
      val tpe1 = p.tpe.iterableArgument
      val fun = '{ ${findCodec(tpe1)}.read(${is}) }
      val addOneApply = Apply(
        Select(readRef, readRef.tpe.termSymbol.method("addOne").head)
      , List(fun.unseal)
      ).seal
      putLimit(
        is
      , addOneApply
      )
    else
      putLimit(
        is
      , Assign(readRef, '{Some(${findCodec(p.tpe)}.read(${is}))}.unseal).seal 
      )

  private def putLimit(is: Expr[CodedInputStream], read: Expr[Any]): Expr[Unit] =
    '{
      val readSize: Int = ${is}.readRawVarint32
      val limit = ${is}.pushLimit(readSize)
      ${read}
      ${is}.popLimit(limit)
    }

  private def initValDef(field: FieldInfo)(using ctx: Context): (ValDef, Ident) =
    if field.tpe.isOption then
      // val pType = field.tpe.seal.asInstanceOf[quoted.Type[Any]]
      // val _none = '{ None:${pType} }.unseal
      val _none = Typed(Ref(NoneModule), field.tpt)
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", field.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref
    else if field.tpe.isIterable then
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
    else
      val pType = field.tpe.seal.asInstanceOf[quoted.Type[Any]]
      val _none = '{ None:Option[${pType}] }.unseal
      val sym = Symbol.newVal(ctx.owner, s"${field.name}Read", _none.tpe, Flags.Mutable, Symbol.noSymbol)
      val init = ValDef(sym, Some(_none))
      val ref = Ref(sym).asInstanceOf[Ident]
      init -> ref

  private def resTerm(ref: Ident, field: FieldInfo): Term =
    if field.tpe.isOption then ref
    else if field.tpe.isIterable then
      Select(ref, ref.tpe.termSymbol.method("result").head)
    else
      val error = s"missing required field `${field.name}: ${field.tpe.typeSymbol.name}`"
      val exeption = '{ throw new RuntimeException(${Expr(error)}) }.unseal
      Apply(
        TypeApply(
          Select(ref, OptionClass.method("getOrElse").head)
        , List(field.tpt)
        )
      , List(exeption)
      )

  private def findCodec(t: Type): Expr[MessageCodec[Any]] = 
    val tpe = t.seal.asInstanceOf[quoted.Type[Any]]
    val msgCodecTpe = '[MessageCodec[$tpe]]
    summonExpr(using msgCodecTpe) match
      case Some(expr) => expr
      case None => qctx.throwError(s"could not find implicit codec for `${tpe.show}`")

  private def classApply(t: Type, params: List[Term]): Term =
    t match
      case y: TermRef => Ident(y)
      case x @ TypeRef(_) =>
        val sym = x.typeSymbol
        val applyMethod = sym.companionModule.method("apply").head
        Apply(Select(Ref(sym.companionModule), applyMethod), params)

  private def increment(x: Ref, y: Expr[Int]): Assign =
    Assign(x, '{ ${x.seal.cast[Int]} + ${y} }.unseal)

}