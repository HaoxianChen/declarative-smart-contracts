package verification

import com.microsoft.z3.{ArithSort, BoolSort, Context, Expr, Model, Quantifier, Status}

object Prove {
   def get_vars(g: Expr[_]): Set[Expr[_]] = {
     if (g.isConst) {
       Set(g)
     }
     else if (g.isApp) {
       val args = g.getArgs
       args.flatMap(get_vars).toSet
     }
     else if (g.isQuantifier) {
       get_vars(g.asInstanceOf[Quantifier].getBody)
     }
     else {
       Set()
     }
   }

   def get_clause(ctx: Context, body: Expr[BoolSort], head:Expr[BoolSort]): Expr[BoolSort] = {
     val vars = get_vars(body).toArray
     val implies = ctx.mkImplies(body, head)
     ctx.mkForall(vars, implies, 1, null, null,
       ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1")
     )
   }

   def prove(ctx: Context, f: Expr[BoolSort]): (Status, Option[Model]) = {
     val solver = ctx.mkSolver()

     val p = ctx.mkParams()
     p.add("timeout", 1000)
     solver.setParameters(p)

     solver.add(ctx.mkNot(f))
     val res = solver.check()
     val model = if (res == Status.SATISFIABLE) {
       Some(solver.getModel)
     }
     else {
       None
     }
     (res, model)
   }

  def testSimplification(): Unit = {
    val ctx = new Context()

    val x = ctx.mkIntConst("x")
    val y = ctx.mkIntConst("y")
    val z = ctx.mkIntConst("z")

    val e = ctx.mkAnd(ctx.mkEq(x,y), ctx.mkEq(y,z))
    println(e.simplify())
  }

  def testQuantifier(): Unit = {
    val ctx: Context = new Context()

    val intSort = ctx.getIntSort

    val a = ctx.mkArrayConst("a1", intSort, intSort)
    val i = ctx.mkIntConst("i")
    val init = ctx.mkForall(Array(i), ctx.mkEq(ctx.mkSelect(a,i), ctx.mkInt(0)),
        1,null,null,ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1"))

    val solver = ctx.mkSolver()
    val f = ctx.mkForall(Array(i), ctx.mkLe(ctx.mkSelect(a,i), ctx.mkInt(1)),
      1,null,null,ctx.mkSymbol("Q2"), ctx.mkSymbol("skid2"))

    solver.add(init)
    solver.add(f)
    val res = solver.check()
    val model = solver.getModel
    println(res)
    println(model)

    ctx.close()

  }

  def testZ3(): Unit = {
    val ctx: Context = new Context()
    /* do something with the context */

    val x = ctx.mkIntConst("x")
    val y = ctx.mkConst("y", ctx.getIntSort)
    val p = ctx.mkAnd(ctx.mkGe(x,ctx.mkInt(5)),
      ctx.mkLe(y,ctx.mkInt(4)))
    val q = ctx.mkGe(x,y)

    val (res,_) = prove(ctx,ctx.mkImplies(p,q))
    println(res)

    /* be kind to dispose manually and not wait for the GC. */
    ctx.close()
  }

  def testTuple(): Unit = {
    val ctx: Context = new Context()
    val tupleSort = ctx.mkTupleSort(ctx.mkSymbol("tuple"),
      Array(ctx.mkSymbol("a"), ctx.mkSymbol("b")),
      Array(ctx.getIntSort, ctx.getIntSort))

    val tuple = ctx.mkConst("t1", tupleSort)
    val n1 = tupleSort.getFieldDecls.apply(0).apply(tuple)
    val n2 = tupleSort.getFieldDecls.apply(1).apply(tuple)

    val solver = ctx.mkSolver()
    val q1 = ctx.mkEq(n1, ctx.mkInt(3))
    val q2 = ctx.mkGt(n2.asInstanceOf[Expr[ArithSort]], ctx.mkInt(9))
    println(q1)
    println(q2)
    solver.add(q1)
    solver.add(q2)
    solver.check()
    println(solver.getModel())
  }
}
