package com.github.dlomsak.regex.deriv

sealed case class Derivations(byChar: Map[Char, RegexAST], default: RegexAST)

sealed trait RegexAST {
    /**
    * denotes whether the regex matches the empty string (helper function for eval and derive)
    */
  def acceptsEmpty: Boolean

  /**
    * denotes whether the node is EmptyAST (used for convenience in property tests)
    */
  def isEmpty: Boolean = false

  /**
    * denotes whether the node is NullAST (used for convenience in property tests)
    */
  def isNull: Boolean = false

  /**
    * determines whether a given input string matches the given regular expression
    */
  def matches(input: String): Boolean = this(input).acceptsEmpty

  def derivations: Derivations

  /**
    * Computes the derivative of a regex with respect to a single character. That is, the regex that matches the
    * remainder of the input given c is consumed. The expression is also simplified along the way.
    */
  def derive(c: Char): RegexAST = {
    val r = derivations.byChar.getOrElse(c, derivations.default)
    r
  }


  /**
    * perform derivation on the expression for a string of characters
    */
  def apply(input: String) = input.foldLeft(this)((r, c) => r.derive(c))

  /*
   * returns the character equivalnce classes per section 4.2
   */
  def getCharClasses: Set[CharClassAST]
}

// AST of regex matching no strings
case object NullAST extends RegexAST {
  val acceptsEmpty = false

  override val isNull: Boolean = true

  val derivations = Derivations(Map.empty, this)

  val getCharClasses = Set(CharClassAST.sigma)
}

// AST of regex matching exactly the empty string
case object EmptyAST extends RegexAST {
  val acceptsEmpty = true

  override val isEmpty: Boolean = true

  val derivations = Derivations(Map.empty, NullAST)

  val getCharClasses = Set(CharClassAST(Set.empty, inverted = true))
}

// Complement of another AST
final class ComplementAST(val re: RegexAST) extends RegexAST {
  val acceptsEmpty = !re.acceptsEmpty

  val derivations = Derivations(re.derivations.byChar.mapValues(ComplementAST.apply), ComplementAST(re.derivations.default))

  val getCharClasses = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case ComplementAST(r2) if re == r2 => true
    case _ => false
  }

  override def toString: String = s"ComplementAST($re)"
}

object ComplementAST {
  def apply(re: RegexAST): RegexAST = re match {
    case ComplementAST(r) => r
    case _ => new ComplementAST(re)
  }

  def unapply(arg: ComplementAST): Option[RegexAST] = Some(arg.re)
}

final class OrAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  val acceptsEmpty = left.acceptsEmpty || right.acceptsEmpty

  val derivations = {
    val domain = left.derivations.byChar.keySet ++ right.derivations.byChar.keySet
    val deriv = domain.toSeq.map { c => c -> OrAST(left.derive(c), right.derive(c)) }
    Derivations(deriv.toMap, NullAST)
  }

  val getCharClasses = CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)

  override def equals(o: Any): Boolean = o match {
    case OrAST(l, r) if (left == l && right == r) || (left == r && right == l) => true
    case _ => false
  }

  override val toString: String = s"OrAST($left, $right)"
}

/*
 * factory methods for operators to ensure ASTs are constructed in normalized form described in section 4.1
 */
object OrAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => right
    case (_, NullAST) => left
    case (ComplementAST(NullAST), r) => ComplementAST(NullAST)
    case (r, ComplementAST(NullAST)) => ComplementAST(NullAST)
    case (OrAST(r, s), t) => new OrAST(r, OrAST(s, t))
    case (l, r) if l==r => l
    case _ => new OrAST(left, right)
  }

  def unapply(arg: OrAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}

final class AndAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  val acceptsEmpty = left.acceptsEmpty && right.acceptsEmpty

  val derivations = {
    val domain = left.derivations.byChar.keySet ++ right.derivations.byChar.keySet
    val deriv = domain.toSeq.map { c => c -> AndAST(left.derive(c), right.derive(c)) }
    Derivations(deriv.toMap, NullAST)
  }

  val getCharClasses = CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)

  override def equals(o: Any): Boolean = o match {
    case AndAST(l, r) if (left == l && right == r) || (left == r && right == l) => true
    case _ => false
  }

  override val toString: String = s"AndAST($left, $right)"
}

object AndAST {
  def apply(left: RegexAST, right: RegexAST): RegexAST = (left, right) match {
    case (NullAST, _) => NullAST
    case (_, NullAST) => NullAST
    case (ComplementAST(NullAST), r) => r
    case (r, ComplementAST(NullAST)) => r
    case (AndAST(r, s), t) => new AndAST(r, AndAST(s, t))
    case (l, r) if l==r => l
    case _ => new AndAST(left, right)
  }

  def unapply(arg: AndAST): Option[(RegexAST, RegexAST)] = Some(arg.left, arg.right)
}

final class CatAST(val left: RegexAST, val right: RegexAST) extends RegexAST {
  val acceptsEmpty = left.acceptsEmpty && right.acceptsEmpty

  val derivations = {
    if (left.acceptsEmpty) {
      val domain = left.derivations.byChar.keys ++ right.derivations.byChar.keys
      val deriv = domain.toSeq.map { c => c -> OrAST(CatAST(left.derive(c), right), right.derive(c)) }
      Derivations(deriv.toMap, NullAST)
    } else {
      Derivations(left.derivations.byChar.mapValues(v => CatAST(v, right)), NullAST)
    }
  }

  val getCharClasses = if (!left.acceptsEmpty) {
    left.getCharClasses
  } else {
    CharClassAST.conjunction(left.getCharClasses, right.getCharClasses)
  }

  override def equals(o: Any): Boolean = o match {
    case CatAST(l, r) if left == l && right == r => true
    case _ => false
  }

  override val toString: String = s"CatAST($left, $right)"
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
  val acceptsEmpty = true

  val derivations = Derivations(re.derivations.byChar.mapValues(r => CatAST(r, this)), NullAST)

  val getCharClasses = re.getCharClasses

  override def equals(o: scala.Any): Boolean = o match {
    case StarAST(r2) if re == r2 => true
    case _ => false
  }

  override val toString: String = s"StarAST($re)"
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
  val acceptsEmpty = false

  val derivations = Derivations(Map(c -> EmptyAST), NullAST)

  val getCharClasses = Set(CharClassAST(Set(c), inverted = false), CharClassAST(Set(c), inverted = true))
}

final case class CharClassAST(chars: Set[Char], inverted: Boolean) extends RegexAST {
  val acceptsEmpty = false

  val derivations = {
    val (present, absent) = if (inverted) (NullAST, EmptyAST) else (EmptyAST, NullAST)
    Derivations(chars.toSeq.map(c => c -> present).toMap, absent)
  }

  def getCharClasses = Set(this, this.copy(inverted = !inverted))

  def acceptsChar(c: Char): Boolean = chars.contains(c) && !inverted || !chars.contains(c) && inverted

  def intersect(other: CharClassAST): CharClassAST = {
    if (!inverted && !other.inverted) {
      CharClassAST(chars.intersect(other.chars), inverted = false)
    } else if (!inverted && other.inverted) {
      CharClassAST(chars.diff(other.chars), inverted = false)
    } else if (inverted && !other.inverted) {
      CharClassAST(other.chars.diff(chars), inverted = false)
    } else {
      CharClassAST(chars.union(other.chars), inverted = true)
    }
  }
}

object CharClassAST {
  val sigma: CharClassAST = CharClassAST(Set.empty, inverted = true)

  def conjunction(left: Set[CharClassAST], right: Set[CharClassAST]): Set[CharClassAST] = left flatMap { x =>
    right.map(_.intersect(x))
  }
}