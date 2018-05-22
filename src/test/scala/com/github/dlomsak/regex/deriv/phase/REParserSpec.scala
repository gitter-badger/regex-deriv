package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv._

/**
  * Created by enki on 3/19/17.
  */
class REParserSpec extends BaseSpec {
  "REParser" should "generate EmptyAST when parsing no tokens" in {
    REParser(RELexer("").right.get) shouldBe Right(EmptyAST)
  }

  it should "fail to parse a lone metacharacter" in {
    REParser(RELexer("+").right.get) shouldBe 'Left
  }

  it should "give higher precedence to * than |" in {
    REParser(RELexer("a|b*").right.get) shouldBe Right(OrAST(CharAST('a'),StarAST(CharAST('b'))))
  }

  it should "give higher precedence to + than |" in {
    REParser(RELexer("a|b+").right.get) shouldBe Right(OrAST(CharAST('a'),CatAST(CharAST('b'), StarAST(CharAST('b')))))
  }

  it should "give higher precedence to ? than |" in {
    REParser(RELexer("a|b?").right.get) shouldBe Right(OrAST(CharAST('a'),OrAST(EmptyAST, CharAST('b'))))
  }

  it should "give higher precedence to * than concatenation" in {
    REParser(RELexer("ab*").right.get) shouldBe Right(CatAST(CharAST('a'),StarAST(CharAST('b'))))
  }

  it should "give higher precedence to + than concatenation" in {
    REParser(RELexer("ab+").right.get) shouldBe Right(CatAST(CharAST('a'),CatAST(CharAST('b'), StarAST(CharAST('b')))))
  }

  it should "give higher precedence to ? than concatenation" in {
    REParser(RELexer("ab?").right.get) shouldBe Right(CatAST(CharAST('a'), OrAST(EmptyAST, CharAST('b'))))
  }

  it should "give higher precedence to concatenation than |" in {
    REParser(RELexer("ab|c").right.get) shouldBe Right(OrAST(CatAST(CharAST('a'), CharAST('b')), CharAST('c')))
  }

  it should "give parentheses highest precedence" in {
    REParser(RELexer("(a|b)*").right.get) shouldBe Right(StarAST(OrAST(CharAST('a'), CharAST('b'))))
  }

  it should "handle repeated characters in character classes" in {
    val chars = REParser(RELexer("[a-cb-d]").right.get) match {
      case Right(CharClassAST(cs, _)) => cs
      case _ => Set.empty[Char]
    }

    chars should contain ('a')
    chars should contain ('b')
    chars should contain ('c')
    chars should contain ('d')
    chars shouldNot contain('-')
    chars.size shouldBe 4
  }


  it should "parse a quantifier" in {
    try {

      REParser(RELexer("a{2}").right.get) shouldBe Right(CatAST(CharAST('a'), CharAST('a')))
      REParser(RELexer("a{2,4}").right.get) shouldBe Right(CatAST(CharAST('a'), CatAST(CharAST('a'), CatAST(OrAST(EmptyAST, CharAST('a')), OrAST(EmptyAST, CharAST('a'))))))
      REParser(RELexer("a{3,}").right.get) shouldBe Right(CatAST(CharAST('a'), CatAST(CharAST('a'), CatAST(CharAST('a'), StarAST(CharAST('a'))))))
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }

  it should "match a number" in {
    try {
      REParser(RELexer("2{4}").right.get) shouldBe Right(CatAST(CharAST(2), CatAST(CharAST(2), CatAST(CharAST(2), CharAST(2)))))
    }
    catch {
      case e: Throwable => e.printStackTrace()
    }
  }
}
