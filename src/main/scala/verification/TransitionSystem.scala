package verification

import com.microsoft.z3.{BitVecSort, BoolExpr, BoolSort, Context, Expr, Sort, Status}
import verification.Prove.{prove}
import verification.TransitionSystem.makeStateVar

case class TransitionSystem(name: String, ctx: Context) {
  private var variables: Set[(Expr[_], Expr[_])] = Set()
  private var init: BoolExpr = ctx.mkTrue()
  private var tr: BoolExpr = ctx.mkTrue()
  private var trs: Set[BoolExpr] = Set()

  def setInit(_init: BoolExpr): Unit = init = _init
  def setTr(_tr: BoolExpr, _trs: Set[BoolExpr]): Unit = {
    tr = _tr
    trs = _trs
  }

  def getInit(): BoolExpr = init
  def getTr(): BoolExpr = tr
  def getTrs(): Set[BoolExpr] = trs

  def newVar[T<:Sort](name: String, sort: T): (Expr[T], Expr[T]) = {
    val (v_in,v_out) = makeStateVar(ctx, name, sort)
    variables += Tuple2(v_in, v_out)
    (v_in,v_out)
  }

  /** Change every variable in f into post variable */
  def toPost(f: Expr[BoolSort]): Expr[BoolSort] = {
    val vs = variables.toArray
    f.substitute(vs.map(_._1), vs.map(_._2))

    /** Rename the remaining free variables */

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
    val amount = ctx.mkBVConst("amount", bvSize)

    /** Variables */
    val (totalSuuply, totalSupplyOut) = tr.newVar("totalSupply", bvSort)
    val (totalBalance, totalBalanceOut) = tr.newVar("totalBalance", bvSort)
    val (balances, balancesOut) = tr.newVar("balances", ctx.mkArraySort(bvSort, bvSort))
    val (allowances, allowancesOut) = tr.newVar("allowances", ctx.mkArraySort(Array(bvSort, bvSort).asInstanceOf[Array[Sort]], bvSort))

    /** Transitions */
    val init = ctx.mkAnd(
      ctx.mkEq(totalSuuply,ctx.mkBV(0, bvSize)),
      ctx.mkEq(totalBalance,ctx.mkBV(0, bvSize)),
      ctx.mkForall(Array(p), ctx.mkEq(ctx.mkSelect(balances,p), ctx.mkBV(0,bvSize)),
                  1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1")),
      ctx.mkForall(Array(p,q), ctx.mkEq(ctx.mkSelect(allowances, Array(p, q).asInstanceOf[Array[Expr[_]]]), ctx.mkBV(0,bvSize)),
                  1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1"))
    )

    // val trMint = ctx.mkAnd(
    //     ctx.mkEq(totalSupplyOut, ctx.mkAdd(totalSuuply, amount)),
    //     ctx.mkGt(amount,ctx.mkInt(0)),
    //   ctx.mkLt(amount,ctx.mkInt(0)),
    //   ctx.mkEq(balancesOut,
    //           ctx.mkStore(balances,p,ctx.mkAdd(ctx.mkSelect(balances,p), amount)))
    // )
    val n = ctx.mkBVConst("n", bvSize)
    val trTransferFrom = ctx.mkAnd(
      ctx.mkBVSGT(amount, ctx.mkBV(0,bvSize)),
      ctx.mkEq(allowancesOut, ctx.mkStore(allowances, Array(p,q).asInstanceOf[Array[Expr[_]]],
        ctx.mkBVSub(ctx.mkSelect(allowances, Array(p,q).asInstanceOf[Array[Expr[_]]]), amount))
      ),
      ctx.mkEq(balancesOut,
        ctx.mkStore(
          ctx.mkStore(balances, q, ctx.mkBVAdd(ctx.mkSelect(balances, q), amount)),
          p, ctx.mkBVSub(ctx.mkSelect(balances, p), amount))
      ),
      ctx.mkEq(totalSupplyOut, totalSuuply),
      ctx.mkEq(n, ctx.mkBVMul(ctx.mkBV(-1, bvSize), amount)),
      ctx.mkEq(totalBalanceOut, ctx.mkBVAdd(ctx.mkBVAdd(totalBalance, amount), n))
    )

    val trSetAllowance = ctx.mkAnd(
      ctx.mkEq(allowancesOut, ctx.mkStore(allowances, Array(p,q).asInstanceOf[Array[Expr[_]]],
        ctx.mkBVAdd(ctx.mkSelect(allowances, Array(p,q).asInstanceOf[Array[Expr[_]]]),
          n))
      ),
      ctx.mkEq(balancesOut, balances),
      ctx.mkEq(totalSupplyOut, totalSuuply),
      ctx.mkEq(totalBalanceOut, totalBalance)
    )

    tr.setInit(init)
    tr.setTr(ctx.mkOr(trTransferFrom, trSetAllowance), Set(trTransferFrom,trSetAllowance))
    // tr.setTr(trMint)

    // val property = ctx.mkNot(ctx.mkExists(
    //                           Array(p,n),
    //                           ctx.mkAnd( ctx.mkEq(ctx.mkSelect(balances,p), n), ctx.mkBVSLT(n,ctx.mkBV(0, bvSize))),
    //                            1, null, null, ctx.mkSymbol("Q2"), ctx.mkSymbol("skid2")))
    val property = ctx.mkNot(ctx.mkExists(
                              Array(p,q),
                              ctx.mkAnd( ctx.mkEq(totalSuuply,p), ctx.mkEq(totalBalance,q),
                                ctx.mkNot(ctx.mkEq(p,q))
                              ),
                               1, null, null, ctx.mkSymbol("Q2"), ctx.mkSymbol("skid2")))
    // val property = ctx.mkEq(totalSuuply, totalBalance)

    // val res = tr.inductiveProve(ctx, property)
    // println(res)

  }

  def makeStateVar[T<:Sort](ctx: Context, name: String, sort: T): (Expr[T], Expr[T]) = {
    // val v_in = ctx.mkConst(s"v_${name}_in", sort)
    val v_in = ctx.mkConst(s"${name}", sort)
    val v_out = ctx.mkConst(s"${name}_next", sort)
    (v_in,v_out)
  }

}
