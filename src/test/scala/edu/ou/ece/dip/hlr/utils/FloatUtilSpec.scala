package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.SpecificationBase

class FloatUtilSpec extends SpecificationBase {
  "If difference is greater than EPSILON then not equal" in {
    FloatUtil.Equals(0.1, 0.1 + FloatUtil.EPSION * 1.1) mustBe false
  }

  "If difference is smaller than EPSILON then is equal" in {
    FloatUtil.Equals(0.1, 0.1 + FloatUtil.EPSION * 0.9) mustBe true
  }

  //todo check the grammar
  "If a number is greater than another by EPSION, then it's greater" in {
    FloatUtil.GreaterThan(0.1 + FloatUtil.EPSION * 1.1, 0.1) mustBe true
  }

  //todo check the grammar
  "If a number is greater than another by less than EPSION, then it's not greater" in {
    FloatUtil.GreaterThan(0.1 + FloatUtil.EPSION * 0.9, 0.1) mustBe false
  }

  //todo check the grammar
  "If a number is smaller than another by less than EPSION, then it's not smaller" in {
    FloatUtil.LessThan(0.1, 0.1 + FloatUtil.EPSION * 0.9) mustBe false
  }

  //todo check the grammar
  "If a number is smaller than another by more than EPSION, then it's smaller" in {
    FloatUtil.LessThan(0.1, 0.1 + FloatUtil.EPSION * 1.1) mustBe true
  }
}