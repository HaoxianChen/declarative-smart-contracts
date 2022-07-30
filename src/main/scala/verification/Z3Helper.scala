package verification

import com.microsoft.z3.{ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort, Symbol, TupleSort}
import datalog.{Add, Arithmetic, Assign, BinFunctor, BinaryOperator, Constant, Equal, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, One, Param, Parameter, Relation, ReservedRelation, Send, SimpleRelation, SingletonRelation, Sub, Type, Unequal, Variable, Zero}

object Z3Helper {
  val uintSize: Int = 32
  val addressSize: Int = 16

  def typeToSort(ctx: Context, t: Type): Sort = t.name match {
    case "address" => ctx.mkBitVecSort(addressSize)
    case "int" => ctx.mkIntSort()
    case "uint" => ctx.mkBitVecSort(uintSize)
    case _ => ???
  }

  def makeTupleSort(ctx: Context, name: String, types: Array[Type]): TupleSort = {
    val sorts = types.map(t => typeToSort(ctx, t))
    val symbols: Array[Symbol] = types.map(t => ctx.mkSymbol(t.name))
    ctx.mkTupleSort(ctx.mkSymbol(name), symbols, sorts)
  }

  def paramToConst(ctx: Context, param: Parameter, prefix: String): (Expr[_<:Sort], Sort) = {
    val sort = typeToSort(ctx, param._type)
    val _newX = param match {
      case Constant(_type, name) => {
        _type.name match {
          case "address" => ctx.mkBV(name.toInt, addressSize)
          case "int" => ctx.mkInt(name.toInt)
          case "uint" => ctx.mkBV(name.toInt, uintSize)
          case _ => ???
        }
      }
      case Variable(_,name) => {
        ctx.mkConst(s"${prefix}_${name}", sort)

      }
    }
    (_newX, sort)
  }

  def fieldsToConst(ctx: Context, fields: List[Parameter], prefix: String): (Expr[_], Sort) = {
    if (fields.size==1) {
      paramToConst(ctx, fields.head, prefix)
    }
    else {
      val fieldTypes = fields.map(_._type)
      val fieldTypeNames = fieldTypes.mkString(",")
      val tupleSortName: String = s"Tuple($fieldTypeNames)"
      val tupleSort = makeTupleSort(ctx, tupleSortName, fieldTypes.toArray)
      val params = fields.map(f => paramToConst(ctx, f, prefix)._1).toArray
      (tupleSort.mkDecl().apply(params:_*), tupleSort)
    }
  }

  def literalToConst(ctx: Context, lit: Literal, indices: List[Int], prefix: String): BoolExpr = {
    lit.relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = indices.map(i => lit.fields(i))
        val values = lit.fields.filterNot(f => keys.contains(f))
        if (keys.nonEmpty) {
          val (valueConst, _) = fieldsToConst(ctx,values,prefix)
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
        val (x,_) = fieldsToConst(ctx, lit.fields,prefix)
        val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
        ctx.mkEq(relConst, x)
      }
      case reserved: ReservedRelation => {
        reserved match {
          case MsgSender() | MsgValue() | Now() => {
            val (x,_) = fieldsToConst(ctx, lit.fields, prefix)
            val relConst = ctx.mkConst(lit.relation.name, getSort(ctx, lit.relation, indices))
            ctx.mkEq(relConst, x)
          }
          case Send() => ???
        }
      }
    }
  }

  def getArraySort(ctx: Context, keyTypes: List[Type], valueTypes: List[Type]): Sort = {
    val keySorts = keyTypes.toArray.map(t => typeToSort(ctx,t))
    val valueSort = if  (valueTypes.size == 1) {
      typeToSort(ctx,valueTypes.head)
    }
    else {
      val fieldNames = valueTypes.mkString(",")
      makeTupleSort(ctx, s"Tuple($fieldNames)", valueTypes.toArray)
    }
    ctx.mkArraySort(keySorts, valueSort)
  }

  def getSort(ctx: Context, relation: Relation, indices: List[Int]): Sort = relation match {
    case rel: SimpleRelation => {
      val keyTypes = indices.map(i => rel.sig(i))
      val valueTypes = rel.sig.filterNot(t => keyTypes.contains(t))
      getArraySort(ctx, keyTypes, valueTypes)
    }
    case SingletonRelation(name, sig, _) => {
      val t = sig.head
      typeToSort(ctx, t)
    }
    case reserved :ReservedRelation => reserved match {
      case MsgSender() | MsgValue() | Now() => typeToSort(ctx, reserved.sig.head)
      case Send() => ???
    }
  }

  def arithmeticToZ3(ctx: Context, arithmetic: Arithmetic, prefix: String): Expr[_] = {
    val sort = typeToSort(ctx, arithmetic._type)
    arithmetic match {
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
        arithmeticToZ3(ctx,Sub(Zero(e._type),e), prefix)
      }
      case operator: BinaryOperator => {
        require(operator.a._type==operator.b._type)
        val _type = operator.a._type
        val x = arithmeticToZ3(ctx, operator.a, prefix)
        val y = arithmeticToZ3(ctx, operator.b, prefix)
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
        }
      }
    }
  }

  def functorToZ3(ctx: Context, binFunctor: BinFunctor, prefix: String): BoolExpr = {
    require(binFunctor.a._type == binFunctor.b._type)
    val _type = binFunctor.a._type

    val x = arithmeticToZ3(ctx, binFunctor.a, prefix)
    val y = arithmeticToZ3(ctx, binFunctor.b, prefix)

    binFunctor match {
      case _:Greater => _type.name match {
        case "int" => ctx.mkGt(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVUGE(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Lesser => _type.name match {
        case "int" => ctx.mkLt(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVULT(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Geq => _type.name match {
        case "int" => ctx.mkGe(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVUGE(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Leq => _type.name match {
        case "int" => ctx.mkLe(x.asInstanceOf[Expr[ArithSort]],y.asInstanceOf[Expr[ArithSort]])
        case "uint" => ctx.mkBVULE(x.asInstanceOf[Expr[BitVecSort]],y.asInstanceOf[Expr[BitVecSort]])
      }
      case _:Unequal => ctx.mkNot(ctx.mkEq(x,y))
      case _:Equal => ctx.mkEq(x,y)
      case _:Assign => ctx.mkEq(x,y)
    }
  }

}
