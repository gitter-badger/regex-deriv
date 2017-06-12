package com.github.dlomsak.regex.deriv

sealed trait RegexAST {
    /**
    * denotes whether the regex matches the empty string (helper function for eval and derive)
    */
  def acceptsEmpty: Boolean

  /**
    * determines whether a given input string matches the given regular expression
    */
  def matches(input: String): Boolean = this(input).acceptsEmpty

  /**
    * Computes the derivative of a regex with respect to a single character. That is, the regex that matches the
    * remainder of the input given c is consumed. The expression is also simplified along the way.
    */
  def derive(c: Char): RegexAST

  /**
    * perform derivation on the expression for a string of characters
    */
  def apply(input: String) = input.foldLeft(this)((r, c) => r.derive(c))
}

// AST of regex matching no strings
case object NullAST extends RegexAST {
  def acceptsEmpty = false

  def derive(c: Char) = this
}

// AST of regex matching exactly the empty string
case object EmptyAST extends RegexAST {
  def acceptsEmpty = true

  def derive(c: Char) = NullAST
}

final class OrAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  def acceptsEmpty = left.acceptsEmpty || right.acceptsEmpty

  def derive(c: Char) = OrAST(left.derive(c), right.derive(c))

  override def equals(o: Any): Boolean = o match {
    case OrAST(l, r) if (left == l && right == r) || (left == r && right == l) => true
    case _ => false
  }

  override def toString: String = s"OrAST($left, $right)"
}

/*
 * factory methods for operators to ensure ASTs are constructed in normalized form described in section 4.1
 */
object OrAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => right
    case (_, NullAST) => left
    case (l, r) if l==r => l
    case (OrAST(r, s), t) => new OrAST(r, OrAST(s, t))
    case _ => new OrAST(left, right)
  }

  def unapply(arg: OrAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}

final class CatAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  def acceptsEmpty = left.acceptsEmpty && right.acceptsEmpty

  def derive(c: Char) = {
    val dLeft = CatAST(left.derive(c), right)
    if (left.acceptsEmpty) {
      OrAST(dLeft, right.derive(c))
    } else {
      dLeft
    }
  }

  override def equals(o: Any): Boolean = o match {
    case CatAST(l, r) if (left == l && right == r) || (left == r && right == l) => true
    case _ => false
  }

  override def toString: String = s"CatAST($left, $right)"
}

object CatAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => NullAST
    case (_, NullAST) => NullAST
    case (EmptyAST, _) => right
    case (_, EmptyAST) => left
    case (CatAST(r, s), t) => new CatAST(r, CatAST(s, t))
    case _ => new CatAST(left, right)
  }

  def unapply(arg: CatAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}


final class StarAST(val re: RegexAST) extends RegexAST {
  def acceptsEmpty = true

  def derive(c: Char) = CatAST(re.derive(c), this)

  override def equals(o: scala.Any): Boolean = o match {
    case StarAST(r2) if re == r2 => true
    case _ => false
  }

  override def toString: String = s"StarAST($re)"
}

object StarAST {
  def apply(re: RegexAST): RegexAST = re match {
    case NullAST => EmptyAST
    case EmptyAST => EmptyAST
    case StarAST(r) => re
    case _ => new StarAST(re)
  }

  def unapply(arg: StarAST): Option[RegexAST] = Some(arg.re)
}


final case class CharAST(c: Char) extends RegexAST {
  def acceptsEmpty = false

  def derive(cin: Char) = if (c == cin) EmptyAST else NullAST
}

final case class CharClassAST(cs: Set[Char], inverted: Boolean) extends RegexAST {
  def acceptsEmpty = cs.isEmpty

  def derive(c: Char) = {
    val isMember = cs.contains(c)
    val isMatch = if (inverted) !isMember else isMember
    if (isMatch) EmptyAST else NullAST
  }
}