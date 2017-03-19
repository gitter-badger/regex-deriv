package com.github.dlomsak.regex.deriv.phase

import com.github.dlomsak.regex.deriv.BaseSpec


class RELexerSpec extends BaseSpec {
  "RELexer" should "succeed but generate no tokens on the empty input" in {
    RELexer("") shouldBe Right(Seq())
  }
}
