package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv._

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class RegexTokenReader(tokens: Seq[RegexToken]) extends Reader[RegexToken] {
  override def first: RegexToken = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[RegexToken] = new RegexTokenReader(tokens.tail)
}

sealed trait RegexAST
case object NullAST extends RegexAST // AST of regex matching no strings
case object EmptyAST extends RegexAST // AST of regex matching exactly the empty string
final case class OrAST(left: RegexAST, right: RegexAST) extends RegexAST
final case class CatAST(left: RegexAST, right: RegexAST) extends RegexAST
final case class StarAST(regex: RegexAST) extends RegexAST
final case class CharAST(c: Char) extends RegexAST

object REParser extends Parsers {
  override type Elem = RegexToken

  def apply(tokens: Seq[RegexToken]): Either[RegexParserError, RegexAST] = {
    val reader = new RegexTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(RegexParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[RegexAST] = phrase(regex)

  def regex: Parser[RegexAST] =
    term ~ ALT ~ regex ^^ { case l ~ _ ~ r => OrAST(l, r) } |
    term

  def term: Parser[RegexAST] = rep1(factor) ^^ {
    case List(r) => r
    case rs => rs.reduceLeft(CatAST)
  }

  def factor: Parser[RegexAST] =
    base <~ STAR ^^ StarAST |
    base <~ PLUS ^^ { b => CatAST(b, StarAST(b)) } |
    base <~ HOOK ^^ { b => OrAST(EmptyAST, b) } |
    base

  def base: Parser[RegexAST] =
    BACKSLASH ~> meta ^^ { x => CharAST(x.asChar)} |
    literal |
    LPAREN ~> regex <~ RPAREN

  def meta:Parser[RegexToken] = LPAREN | RPAREN | LBRACKET | RBRACKET | PLUS | STAR | HOOK | BACKSLASH

  def literal: Parser[CharAST] = {
    accept("character literal", { case lit @ CHARLIT(c) => CharAST(c) })
  }
}
