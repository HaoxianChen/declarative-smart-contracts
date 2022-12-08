package verification

object BuiltInRules {
  /** Built-in rules in Datalog syntax.
   * It is parsed as a regular datalog source file. */
  val ruleStr: String =
    """
      |.decl *receiveTotal(n: uint)
      |.decl *sendTotal(n: uint)
      |
      |receiveTotal(s) :- s = sum n: receive(_,n).
      |sendTotal(s) :- s = sum n: send(_,n).
      |
      |thisBalance(n) :- receiveTotal(m), sendTotal(l), n := m - l.
      |""".stripMargin
}
