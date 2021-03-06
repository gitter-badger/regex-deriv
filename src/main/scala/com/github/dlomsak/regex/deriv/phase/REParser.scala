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
      case Error(msg, next) => Left(RegexParserError(msg))
    }
  }

  def program: Parser[RegexAST] = phrase(opt(regex)) ^^ { _.getOrElse(EmptyAST) }

  def regex: Parser[RegexAST] =
    term ~ opt((PIPE | AMPER) ~ regex) ^^ {
      case l ~ Some(PIPE ~ r) => OrAST(l, r)
      case l ~ Some(AMPER ~ r) => AndAST(l, r)
      case l ~ None => l
    }

  def term: Parser[RegexAST] = rep1(factor) ^^ { _.reduceLeft(CatAST.apply) }

  def factor: Parser[RegexAST] =  base ~ opt(STAR | PLUS | HOOK | quantifier) ^^ {
      case r ~ Some(STAR) => StarAST(r)
      case r ~ Some(PLUS) => CatAST(r, StarAST(r))
      case r ~ Some(HOOK) => OrAST(EmptyAST, r)
      case r ~ Some((lower: Int, optUpper: Option[Int])) =>
        val suffix = optUpper map { upper =>
          (lower until upper).foldLeft(EmptyAST: RegexAST) { case (subtree, _) => CatAST(OrAST(r, EmptyAST), subtree) }
        } getOrElse {
          StarAST(r)
        }
        val prefix = (0 until lower).foldLeft(EmptyAST: RegexAST) { case (subtree, _) => CatAST(r, subtree) }
        CatAST(prefix, suffix)
      case r ~ None => r
  }

  def base: Parser[RegexAST] =
    singleChar |
    DOT ^^ { _ => CharClassAST.sigma } |
    LBRACKET ~> opt(CARET) ~ rep1(charRange) <~ RBRACKET ^^
      { case invert ~ chars => CharClassAST(chars.reduce(_ ++ _), invert.isDefined) } |
    LPAREN ~> regex <~ RPAREN |
    TILDE ~> regex ^^ { case r => ComplementAST(r) }

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

  def literal: Parser[CharAST] = accept("character literal", { case _ @ CHARLIT(c) => CharAST(c) })

  def digitLiteral: Parser[CharAST] = accept("digit lit", { case _ @ DIGITLIT(x) => CharAST(x) })

  def int: Parser[Int] = {
    rep1(accept("int lit", { case i @ DIGITLIT(x) => x})) ^^ { chars => chars.mkString.toInt }
  }


  def quantifier: Parser[(Int, Option[Int])] = LBRACE ~> int ~ opt(COMMA) ~ opt(int) <~ RBRACE flatMap {
    case i ~ Some(_) ~ j if i <= j.getOrElse(Int.MaxValue)  => success((i, j))
    case i ~ None ~ None => success((i, Some(i)))
    case i ~ Some(_) ~ Some(j) => err(s"In quantifier {m,n}, m is $i and n is $j, but m cannot be greater than n.")
  }
}
