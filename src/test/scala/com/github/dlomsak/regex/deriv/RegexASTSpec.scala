package com.github.dlomsak.regex.deriv


class RegexASTSpec extends BaseSpec {
  "CatAST" should "associate to the right" in {
    CatAST(CatAST(CharAST('a'), CharAST('b')), CharAST('c')) shouldBe CatAST(CharAST('a'), CatAST(CharAST('b'), CharAST('c')))
  }

  it should "commute over equality" in {
    CatAST(CharAST('a'), CharAST('b')) shouldEqual CatAST(CharAST('b'), CharAST('a'))
  }

  "OrAST" should "associate to the right" in {
    OrAST(OrAST(CharAST('a'), CharAST('b')), CharAST('c')) shouldBe OrAST(CharAST('a'), OrAST(CharAST('b'), CharAST('c')))
  }

  it should "commute over equality" in {
    OrAST(CharAST('a'), CharAST('b')) shouldEqual OrAST(CharAST('b'), CharAST('a'))
  }
}
