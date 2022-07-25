package verification

import com.microsoft.z3.{BitVecSort, BoolExpr, BoolSort, Context, Expr, Sort}
import verification.Prove.prove_inductive
import verification.TransitionSystem.makeStateVar

case class TransitionSystem(name: String, ctx: Context) {
  var variables: Set[(Expr[_], Expr[_])] = Set()
  var init: BoolExpr = ctx.mkTrue()
  var tr: BoolExpr = ctx.mkTrue()

  def setInit(_init: BoolExpr): Unit = init = _init
  def setTr(_tr: BoolExpr): Unit = tr = _tr

  def newVar[T<:Sort](name: String, sort: T): (Expr[T], Expr[T]) = {
    val (v_in,v_out) = makeStateVar(ctx, name, sort)
    variables += Tuple2(v_in, v_out)
    (v_in,v_out)
  }

  /** Change every variable in f into post variable */
  def toPost(f: Expr[BoolSort]): Expr[BoolSort] = {
    val vs = variables.toArray
    f.substitute(vs.map(_._1), vs.map(_._2))
  }
}

object TransitionSystem {

  def testTS(): Unit = {
    val ctx = new Context()
    val tr = TransitionSystem("wallet", ctx)
    val bvSize = 16
    val bvSort = ctx.mkBitVecSort(bvSize)

    val p: Expr[BitVecSort] = ctx.mkConst("p", bvSort)
    val amount = ctx.mkIntConst("amount")

    /** Variables */
    val (totalSuuply, totalSupplyOut) = tr.newVar("totalSupply", ctx.mkIntSort())
    val (balances, balancesOut) = tr.newVar("balances", ctx.mkArraySort(bvSort, ctx.mkIntSort()))

    /** Transitions */
    val init = ctx.mkAnd(
      ctx.mkEq(totalSuuply,ctx.mkInt(0)),
      ctx.mkForall(Array(p), ctx.mkEq(ctx.mkSelect(balances,p), ctx.mkInt(0)),
                  1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1"))
    )

    val trMint = ctx.mkAnd(
        ctx.mkEq(totalSupplyOut, ctx.mkAdd(totalSuuply, amount)),
        ctx.mkGt(amount,ctx.mkInt(0)),
      ctx.mkLt(amount,ctx.mkInt(0)),
      ctx.mkEq(balancesOut,
              ctx.mkStore(balances,p,ctx.mkAdd(ctx.mkSelect(balances,p), amount)))
    )

    tr.setInit(init)
    tr.setTr(trMint)

    val property = ctx.mkForall(Array(p), ctx.mkGe(ctx.mkSelect(balances,p), ctx.mkInt(0)),
        1, null, null, ctx.mkSymbol("Q2"), ctx.mkSymbol("skid2"))

    val res = prove_inductive(ctx, tr,property)
    println(res)

  }

  def makeStateVar[T<:Sort](ctx: Context, name: String, sort: T): (Expr[T], Expr[T]) = {
    val v_in = ctx.mkConst(s"v_${name}_in", sort)
    val v_out = ctx.mkConst(s"v_${name}_out", sort)
    (v_in,v_out)
  }

}
