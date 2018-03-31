package com.github.dlomsak.regex.deriv

/**
  * A deterministic finite automaton
  */
case class DFA[A](states: Set[A], init: A, accepting: Set[A], delta:Map[A, List[(CharClassAST, A)]]) {
  def accepts(s: String):Boolean = {
    val finalState = s.foldLeft(init) { case (state, c) =>
      val (_, nextState) = delta(state).find{ case (cls, _) => cls.acceptsChar(c) }.get
      nextState
    }
    accepting.contains(finalState)
  }
}
