package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.BaseSpec


class RELexerSpec extends BaseSpec {
  "RELexer" should "succeed but generate no tokens on the empty input" in {
    RELexer("") shouldBe Right(Seq())
  }

  it should "generate all of the defined tokens" in {
    RELexer("""|\a?[(+])*-^""") shouldBe Right(Seq(ALT,BACKSLASH,CHARLIT('a'),HOOK,LBRACKET,LPAREN,PLUS,RBRACKET,RPAREN,STAR,DASH,CARET))
  }
}
