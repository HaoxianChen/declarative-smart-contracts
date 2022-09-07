package verification

import com.microsoft.z3.{ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Quantifier, Sort, Symbol, TupleSort}
import datalog.{Add, ArithOperator, Arithmetic, Assign, Balance, BinaryOperator, Constant, Div, Equal, Functor, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, One, Param, Parameter, Relation, ReservedRelation, Send, SimpleRelation, SingletonRelation, Sub, Type, Unequal, Variable, Zero}

object Z3Helper {
  val uintSize: Int = 32
  val addressSize: Int = 16

  def typeToSort(ctx: Context, t: Type): Sort = t.name match {
    case "address" => ctx.mkBitVecSort(addressSize)
    case "int" => ctx.mkIntSort()
    case "uint" => ctx.mkBitVecSort(uintSize)
    case "bool" => ctx.mkBoolSort()
    case _ => ???
  }

  def makeTupleSort(ctx: Context, relation: Relation, types: Array[Type], fieldNames: Array[String]): TupleSort = {
    require(types.length == fieldNames.length, s"${relation.name}, ${types.mkString(",")}, ${types.length}, ${fieldNames.mkString(",")}, ${fieldNames.length}")
    val sorts = types.map(t => typeToSort(ctx, t))
    val symbols: Array[Symbol] = fieldNames.map(ctx.mkSymbol)
    ctx.mkTupleSort(ctx.mkSymbol(relToTupleName(relation)), symbols, sorts)
  }

  def paramToConst(ctx: Context, param: Parameter, prefix: String): (Expr[_<:Sort], Sort) = {
    val sort = typeToSort(ctx, param._type)
    val _newX = param match {
      case Constant(_type, name) => {
        _type.name match {
          case "address" => ctx.mkBV(name.toInt, addressSize)
          case "int" => ctx.mkInt(name.toInt)
          case "uint" => ctx.mkBV(name.toInt, uintSize)
          case "bool" => name match {
            case "true" => ctx.mkTrue()
            case "false" => ctx.mkFalse()
          }
          case _ => ???
        }
      }
      case Variable(_,name) => {
        ctx.mkConst(s"${prefix}_${name}", sort)

      }
    }
    (_newX, sort)
  }

  def relToTupleName(relation: Relation): String = s"${relation.name}Tuple"

  def fieldsToConst(ctx: Context, relation: Relation, fields: List[Parameter], fieldNames: List[String], prefix: String): (Expr[_], Sort) = {
    require(fields.size == fieldNames.size)
    if (fields.size==1) {
      paramToConst(ctx, fields.head, prefix)
    }
    else {
      val fieldTypes = fields.map(_._type)
      val tupleSort = makeTupleSort(ctx, relation, fieldTypes.toArray, fieldNames.toArray)
      val params = fields.map(f => paramToConst(ctx, f, prefix)._1).toArray
      (tupleSort.mkDecl().apply(params:_*), tupleSort)
    }
  }

  def literalToConst(ctx: Context, lit: Literal, indices: List[Int], prefix: String): BoolExpr = {
    lit.relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = indices.map(i => lit.fields(i))
        val valueIndices = lit.fields.indices.filterNot(i=>indices.contains(i)).toList
        val values = valueIndices.map(i=>lit.fields(i))
        val fieldNames = valueIndices.map(i => lit.relation.memberNames(i))

        if (keys.nonEmpty) {
          val (valueConst, _) = fieldsToConst(ctx, lit.relation, values, fieldNames, prefix)
          val sort = getSort(ctx, lit.relation, indices)
          val arrayConst = ctx.mkConst(name, sort)
          val keyConsts: Array[Expr[_]] = keys.toArray.map(f => paramToConst(ctx, f, prefix)._1)
          ctx.mkEq(ctx.mkSelect(arrayConst.asInstanceOf[Expr[ArraySort[Sort,Sort]]], keyConsts), valueConst)
        }
        else {
          ???
        }
      }
      case SingletonRelation(name, sig, memberNames) => {
        if (memberNames.size==1) {
          val (x,_) = paramToConst(ctx, lit.fields.head, prefix)
          val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
          ctx.mkEq(relConst, x)
        }
        else {
          val tupleSort = getSort(ctx, lit.relation, indices).asInstanceOf[TupleSort]
          val tuple = ctx.mkConst(name, tupleSort)
          matchFieldstoTuple(ctx, tupleSort, tuple, lit.fields, prefix)
        }
      }
      case reserved: ReservedRelation => {
        reserved match {
          case MsgSender() | MsgValue() | Now() | Balance() => {
            val (x,_) = paramToConst(ctx, lit.fields.head, prefix)
            val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
            ctx.mkEq(relConst, x)
          }
          case Send() => ???
        }
      }
    }
  }

  def matchFieldstoTuple(ctx: Context, tupleSort: TupleSort, tuple: Expr[_], fields: List[Parameter], prefix: String): BoolExpr = {
    var eqs: Array[BoolExpr] = Array()
    for ((p, decl) <- fields.zip(tupleSort.getFieldDecls) ) {
      if (p.name != "_") {
        val (pConst, _) = paramToConst(ctx, p, prefix)
        eqs :+= ctx.mkEq(pConst, decl.apply(tuple))
      }
    }
    ctx.mkAnd(eqs:_*)
  }


  def getArraySort(ctx: Context, relation: Relation, indices: List[Int]): (Sort, Array[Sort], Sort) = {
    require(indices.nonEmpty)
    val keyTypes = indices.map(i => relation.sig(i))
    val valueIndices = relation.sig.indices.filterNot(i=>indices.contains(i)).toList
    val valueTypes = valueIndices.map(i=>relation.sig(i))
    val fieldNames = valueIndices.map(i=>relation.memberNames(i))

    val keySorts = keyTypes.toArray.map(t => typeToSort(ctx,t))
    val valueSort = if  (valueTypes.size == 1) {
      typeToSort(ctx,valueTypes.head)
    }
    else {
      makeTupleSort(ctx, relation, valueTypes.toArray, fieldNames.toArray)
    }
    (ctx.mkArraySort(keySorts, valueSort), keySorts, valueSort)
  }


  def getSort(ctx: Context, relation: Relation, indices: List[Int]): Sort = relation match {
    case rel: SimpleRelation => getArraySort(ctx, relation, indices)._1
    case SingletonRelation(name, sig, memberNames) => {
      if (sig.size==1) {
        val t = sig.head
        typeToSort(ctx, t)
      }
      else {
        makeTupleSort(ctx, relation, sig.toArray, memberNames.toArray)
      }
    }
    case reserved :ReservedRelation => reserved match {
      case MsgSender() | MsgValue() | Now() | Balance() => typeToSort(ctx, reserved.sig.head)
      case Send() => ???
    }
  }

  def functorExprToZ3(ctx: Context, expr: datalog.Expr, prefix: String): Expr[_] = {
    val sort = typeToSort(ctx, expr._type)
    expr match {
      case Zero(_type) => _type.name match {
        case "int" => ctx.mkInt(0).asInstanceOf[Expr[ArithSort]]
        case "uint" => ctx.mkBV(0,uintSize).asInstanceOf[Expr[ArithSort]]
        case _ => ???
      }
      case One(_type) => _type.name match {
        case "int" => ctx.mkInt(1).asInstanceOf[Expr[ArithSort]]
        case "uint" => ctx.mkBV(1,uintSize).asInstanceOf[Expr[ArithSort]]
        case _ => ???
      }
      case Param(p) => {
        paramToConst(ctx, p, prefix)._1.asInstanceOf[Expr[ArithSort]]
        // ctx.mkConst(s"${prefix}_${p.name}", sort).asInstanceOf[Expr[ArithSort]]
      }
      case Negative(e) => {
        // assert(e._type.name == "int")
        functorExprToZ3(ctx, Sub(Zero(e._type),e), prefix)
      }
      case operator: BinaryOperator => {
        require(operator.a._type==operator.b._type)
        val _type = operator.a._type
        val x = functorExprToZ3(ctx, operator.a, prefix)
        val y = functorExprToZ3(ctx, operator.b, prefix)
        operator match {
          case _:Add => _type.name match {
            case "int" => ctx.mkAdd(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVAdd(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
          case _:Sub =>_type.name match {
            case "int" => ctx.mkSub(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVSub(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
          case _:Mul => _type.name match {
            case "int" => ctx.mkMul(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVMul(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
          case _:Div => _type.name match {
            case "int" => ctx.mkDiv(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
            case "uint" => ctx.mkBVUDiv(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
          }
        }
      }
    }
  }

  def functorToZ3(ctx: Context, functor: Functor, prefix: String): BoolExpr = functor match {
    case arith: ArithOperator => {
      require(arith.a._type == arith.b._type)
      val _type = arith.a._type

      val x = functorExprToZ3(ctx, arith.a, prefix)
      val y = functorExprToZ3(ctx, arith.b, prefix)

      arith match {
        case _: Greater => _type.name match {
          case "int" => ctx.mkGt(x.asInstanceOf[Expr[ArithSort]], y.asInstanceOf[Expr[ArithSort]])
          case "uint" => ctx.mkBVUGT(x.asInstanceOf[Expr[BitVecSort]], y.asInstanceOf[Expr[BitVecSort]])
        }
        case _: Lesser => _type.name match {
          case "int" => ctx.mkLt(x.asInstanceOf[Expr[ArithSort]], y.asInstanceOf[Expr[ArithSort]])
          case "uint" => ctx.mkBVULT(x.asInstanceOf[Expr[BitVecSort]], y.asInstanceOf[Expr[BitVecSort]])
        }
        case _: Geq => _type.name match {
          case "int" => ctx.mkGe(x.asInstanceOf[Expr[ArithSort]], y.asInstanceOf[Expr[ArithSort]])
          case "uint" => ctx.mkBVUGE(x.asInstanceOf[Expr[BitVecSort]], y.asInstanceOf[Expr[BitVecSort]])
        }
        case _: Leq => _type.name match {
          case "int" => ctx.mkLe(x.asInstanceOf[Expr[ArithSort]], y.asInstanceOf[Expr[ArithSort]])
          case "uint" => ctx.mkBVULE(x.asInstanceOf[Expr[BitVecSort]], y.asInstanceOf[Expr[BitVecSort]])
        }
      }
    }
    case Unequal(a,b) => {
      val x = functorExprToZ3(ctx, a, prefix)
      val y = functorExprToZ3(ctx, b, prefix)
      ctx.mkNot(ctx.mkEq(x,y))
    }
    case Equal(a,b) => {
      val x = functorExprToZ3(ctx, a, prefix)
      val y = functorExprToZ3(ctx, b, prefix)
      ctx.mkEq(x, y)
    }
    case Assign(a,b) => {
      val x = functorExprToZ3(ctx, a, prefix)
      val y = functorExprToZ3(ctx, b, prefix)
      ctx.mkEq(x,y)
    }
  }

  def extractEq(expr: Expr[_], exceptions: Set[Expr[_]]): Array[(Expr[_], Expr[_])] = {
    if (expr.isEq) {
      val args = expr.getArgs
      if (args.forall(a => a.isConst && !exceptions.contains(a))) {
        require(expr.getNumArgs == 2)
        Array(Tuple2(args(0), args(1)))
      }
      else {
        args.flatMap(a => extractEq(a,exceptions))
      }
    }
    else if (expr.isNot) {
      Array()
    }
    else if (expr.isApp) {
      expr.getArgs.flatMap(a => extractEq(a,exceptions))
    }
    else if (expr.isQuantifier) {
      extractEq(expr.asInstanceOf[Quantifier].getBody, exceptions)
    }
    else {
      Array()
    }
  }

  def initValue(ctx: Context, _type: Type): Expr[_<:Sort] = _type.name match {
    case "address" => ctx.mkBV(0, addressSize)
    case "int" => ctx.mkInt(0)
    case "uint" => ctx.mkBV(0, uintSize)
    case "bool" => ctx.mkBool(false)
  }


}
