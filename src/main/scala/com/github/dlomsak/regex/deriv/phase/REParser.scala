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


object REParser extends Parsers {
  override type Elem = RegexToken

  def apply(tokens: Seq[RegexToken]): Either[RegexParserError, RegexAST] = {
    val reader = new RegexTokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) => Left(RegexParserError(msg))
      case Success(result, next) => Right(result)
    }
  }

  def program: Parser[RegexAST] = phrase(opt(regex)) ^^ { _.getOrElse(EmptyAST) }

  def regex: Parser[RegexAST] =
    term ~ opt(ALT ~ regex) ^^ {
      case l ~ Some(_ ~ r) => OrAST(l, r)
      case l ~ None => l
    }

  def term: Parser[RegexAST] = rep1(factor) ^^ { _.reduceLeft(CatAST.apply) }

  def factor: Parser[RegexAST] = {
    val partialFactor = base ~ opt(STAR | PLUS | HOOK) ^^ {
      case r ~ Some(STAR) => StarAST(r)
      case r ~ Some(PLUS) => CatAST(r, StarAST(r))
      case r ~ Some(HOOK) => OrAST(EmptyAST, r)
      case r ~ None => r
    }

    partialFactor ~ opt(quantifier) ^^ {
      case r ~ Some(q) => quantifierToCat(r, q)
      case r ~ None => r
    }
  }

  def quantifierToCat(r: RegexAST, q: (Int, Option[Int])): RegexAST = {
    q match {
      case (1, Some(1)) => r
      case(i, Some(1)) if i < 1 => OrAST(r, EmptyAST)
      case(0, None) => StarAST(r)
      case (i, Some(j)) if i < j && i <= 0 => CatAST(OrAST(r, EmptyAST), quantifierToCat(r, (i-1, Some(j-1))))
      case (i, j) => CatAST(r, quantifierToCat(r, (i-1, j.map{_ - 1})))
    }
  }

  def base: Parser[RegexAST] =
    singleChar |
    DOT ^^ { _ => CharClassAST.sigma } |
    LBRACKET ~> opt(CARET) ~ rep1(charRange) <~ RBRACKET ^^
      { case invert ~ chars => CharClassAST(chars.reduce(_ ++ _), invert.isDefined) } |
    LPAREN ~> regex <~ RPAREN



  def r = opt(literal).filter(_.exists(_.c.isDigit))
  def charRange: Parser[Set[Char]] = {
    literal ~ DASH ~ literal ^^ { case start ~ _ ~ stop => Set(start.c to stop.c:_*) } |
    singleChar ^^ { ch => Set(ch.c) }
  }

  def singleChar: Parser[CharAST] =
    BACKSLASH ~> meta ^^ { x => CharAST(x.asChar) } |
    literal |
    digitLiteral

  def meta: Parser[RegexToken] =
    LPAREN | RPAREN | LBRACKET | RBRACKET | PLUS | STAR | HOOK | BACKSLASH | DOT | CARET | DASH | LBRACE | RBRACE | COMMA

  def literal: Parser[CharAST] = {
    accept("character literal", { case lit @ CHARLIT(c) => CharAST(c) })
  }

  def digitLiteral: Parser[CharAST] = accept("int lit", {case _ @ INTLIT(x) => CharAST(x)})

  def int: Parser[Int] = {
    rep1(accept("int lit", { case i @ INTLIT(x) => x})) ^^ { chars => chars.mkString.toInt}
  }


  def quantifier: Parser[(Int, Option[Int])] = LBRACE ~> int ~ opt(COMMA) ~ opt(int) <~ RBRACE ^^ {
    case i ~ Some(_) ~ j   => (i, j)
    case i ~ None ~ None => (i, Some(i))
  }


}
