package com.github.dlomsak.regex.deriv

sealed abstract class RegexAST[T]
// AST of regex matching no strings
final case class NullAST[T]() extends RegexAST[T]
// AST of regex matching exactly the empty string
final case class EmptyAST[T]() extends RegexAST[T]
// Complement of another AST
final case class ComplementAST[T](re: T) extends RegexAST[T]
final case class OrAST[T](left: T, right: T) extends RegexAST[T]
// intersection of two ASTs
final case class AndAST[T](left: T, right: T) extends RegexAST[T]
final case class CatAST[T](left: T, right: T) extends RegexAST[T]
final case class StarAST[T](re: T) extends RegexAST[T]
final case class CharAST[T](c: Char) extends RegexAST[T]
final case class CharClassAST[T](chars: Set[Char], inverted: Boolean) extends RegexAST[T]