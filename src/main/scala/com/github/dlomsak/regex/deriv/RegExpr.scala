package com.github.dlomsak.regex.deriv

import com.github.dlomsak.regex.deriv.phase._

import scala.annotation.tailrec

case class RegExpr(ast: RegexAST) {
  def apply(input: String): RegExpr = RegExpr(derive(input))

  /**
    * determines whether the given expression matches the empty string (helper function for eval and derive)
    */
  protected def acceptsEmpty(regex: RegexAST = ast):Boolean = regex match {
    case NullAST => false
    case EmptyAST => true
    case CharAST(_) => false
    case OrAST(l, r) => acceptsEmpty(l) || acceptsEmpty(r)
    case CatAST(l, r) => acceptsEmpty(l) && acceptsEmpty(r)
    case StarAST(_) => true
  }

  /**
    * determines whether a given input string matches the given regular expression
    */
  def matches(input: String): Boolean = acceptsEmpty(derive(input))

  /**
    * perform derivation on the expression for a string of characters
    */
  protected def derive(input: String):RegexAST = input.foldLeft(ast)((r, c) => derive(c, r))

  /**
    * Computes the derivative of a regex with respect to a single character. That is, the regex that matches the
    * remainder of the input given c is consumed. The expression is also simplified along the way.
    */
  protected def derive(c: Char, regex: RegexAST = ast): RegexAST = regex match {
    case NullAST => NullAST
    case EmptyAST => NullAST
    case CharAST(c2) => if (c2 == c) EmptyAST else NullAST
    case OrAST(l, r) => (derive(c, l), derive(c, r)) match {
      case (NullAST, NullAST) => NullAST
      case (NullAST, dr) => dr
      case (dl, NullAST) => dl
      case (dl, dr) => OrAST(dl, dr)
    }
    case CatAST(l, r) =>
      // derive on the left, unless the left acceptsEmpty in which case as also OR the result with right's derivative
      val result =  derive(c, l) match {
        case NullAST => NullAST
        case EmptyAST => r
        case dl => CatAST(dl, r)
      }
      if (acceptsEmpty(l)) {
        derive(c, r) match {
          case NullAST => result // don't bother ORing null to result
          case dr => OrAST(result, dr)
        }
      } else {
        result
      }
    case StarAST(r) => derive(c, r) match {
      case NullAST => NullAST
      case EmptyAST => ast
      case dr => CatAST(dr, ast)
    }
  }
}

object RegExpr {
  def apply(regex: String) = for {
    tokens <- RELexer(regex).right
    ast <- REParser(tokens).right
  } yield new RegExpr(ast)
}
