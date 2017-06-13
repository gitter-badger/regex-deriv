package com.github.dlomsak.regex.deriv


class RegexASTSpec extends BaseSpec {

  "CatAST" should "associate to the right" in {
    forAll { (r1: RegexAST, r2: RegexAST, r3: RegexAST) =>
      CatAST(CatAST(r1, r2), r3) should equal (CatAST(r1, CatAST(r2, r3)))
    }
  }

  it should "not generally commute over equality" in {
    forAll { (r1: RegexAST, r2: RegexAST) =>
      whenever(!r1.isEmpty && !r1.isNull && !r2.isEmpty && !r2.isNull && r1 != r2) {
        CatAST(r1, r2) should not equal CatAST(r2, r1)
      }
    }
  }

  "OrAST" should "associate to the right" in {
    forAll { (r1: RegexAST, r2: RegexAST, r3: RegexAST) =>
      OrAST(OrAST(r1, r2), r3) should equal (OrAST(r1, OrAST(r2, r3)))
    }
  }

  it should "commute over equality" in {
    forAll { (r1: RegexAST, r2: RegexAST) =>
      OrAST(r1, r2) should equal (OrAST(r2, r1))
    }
  }

}
