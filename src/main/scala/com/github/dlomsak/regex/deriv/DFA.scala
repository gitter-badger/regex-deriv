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

  def toDot: String = {
    val body = delta.map { case (state,transitions) =>
      transitions.map { case (charCls, toState) =>
        val prefix = if (charCls.inverted) "~" else ""
        val transChars = charCls.chars.mkString(",")
        val label = if (charCls.chars.isEmpty && charCls.inverted)
          "\u03A3"
        else if (charCls.chars.size > 1)
          s"$prefix{$transChars}"
        else
          s"$prefix$transChars"
        s"""$state -> $toState [ label = \"$label\" ];"""
      }  mkString "\n"
    } mkString "\n"
    s"""digraph dfa {
        |node [color = white] "";
        |node [shape = doublecircle, color = black]; ${accepting.mkString(" ")};
        |node [shape = circle];
        |"" -> $init;
        |$body
        |}
     """.stripMargin
  }
}