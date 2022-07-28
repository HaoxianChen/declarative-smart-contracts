package verification

import com.microsoft.z3.{BitVecSort, BoolExpr, BoolSort, Context, Expr, Sort, Status}
import verification.Prove.{prove}
import verification.TransitionSystem.makeStateVar

case class TransitionSystem(name: String, ctx: Context) {
  private var variables: Set[(Expr[_], Expr[_])] = Set()
  private var init: BoolExpr = ctx.mkTrue()
  private var tr: BoolExpr = ctx.mkTrue()

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

  def inductiveProve(ctx: Context, property: BoolExpr): (Status, Status) = {
    val resInit = prove(ctx, ctx.mkImplies(init, property))
    val f2 = ctx.mkImplies(ctx.mkAnd(property, tr), toPost(property))
    val resTr = prove(ctx, f2)
    (resInit, resTr)
  }
}

object TransitionSystem {

  def testTS(): Unit = {
    val ctx = new Context()
    val tr = TransitionSystem("wallet", ctx)
    val bvSize = 16
    val bvSort = ctx.mkBitVecSort(bvSize)

    val p: Expr[BitVecSort] = ctx.mkConst("p", bvSort)
    val q: Expr[BitVecSort] = ctx.mkConst("q", bvSort)
    val amount = ctx.mkIntConst("amount")

    /** Variables */
    val (totalSuuply, totalSupplyOut) = tr.newVar("totalSupply", ctx.mkIntSort())
    val (balances, balancesOut) = tr.newVar("balances", ctx.mkArraySort(bvSort, ctx.mkIntSort()))
    val (allowances, allowancesOut) = tr.newVar("allowances", ctx.mkArraySort(bvSort,
                                                        ctx.mkArraySort(bvSort, bvSort)))

    /** Transitions */
    val init = ctx.mkAnd(
      ctx.mkEq(totalSuuply,ctx.mkInt(0)),
      ctx.mkForall(Array(p), ctx.mkEq(ctx.mkSelect(balances,p), ctx.mkInt(0)),
                  1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1")),
      ctx.mkForall(Array(p,q), ctx.mkEq(ctx.mkSelect(ctx.mkSelect(allowances, p),q), ctx.mkBV(0,bvSize)),
                  1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1"))
    )

    val trMint = ctx.mkAnd(
        ctx.mkEq(totalSupplyOut, ctx.mkAdd(totalSuuply, amount)),
        ctx.mkGt(amount,ctx.mkInt(0)),
      ctx.mkLt(amount,ctx.mkInt(0)),
      ctx.mkEq(balancesOut,
              ctx.mkStore(balances,p,ctx.mkAdd(ctx.mkSelect(balances,p), amount)))
    )

    val trSetAllowance = ctx.mkAnd(ctx.mkEq(allowancesOut, ctx.mkStore(allowances,p,
                                          ctx.mkStore(ctx.mkSelect(allowances,p), q,
                                            ctx.mkBVConst("n", bvSize)))),
      ctx.mkEq(balancesOut, balances),
      ctx.mkEq(totalSupplyOut, totalSuuply))

    tr.setInit(init)
    tr.setTr(ctx.mkOr(trMint, trSetAllowance))
    // tr.setTr(trMint)

    val n = ctx.mkIntConst("n")
    val property = ctx.mkNot(ctx.mkExists(
                              Array(p,n),
                              ctx.mkAnd( ctx.mkEq(ctx.mkSelect(balances,p), n), ctx.mkLt(n,ctx.mkInt(0))),
                               1, null, null, ctx.mkSymbol("Q2"), ctx.mkSymbol("skid2")))

    val res = tr.inductiveProve(ctx, property)
    println(res)

  }

  def makeStateVar[T<:Sort](ctx: Context, name: String, sort: T): (Expr[T], Expr[T]) = {
    // val v_in = ctx.mkConst(s"v_${name}_in", sort)
    val v_in = ctx.mkConst(s"${name}", sort)
    val v_out = ctx.mkConst(s"v_${name}_out", sort)
    (v_in,v_out)
  }

}
