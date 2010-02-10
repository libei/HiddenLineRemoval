package edu.ou.ece.dip.hlr

import org.specs.matcher.xUnit
import shape._
import org.specs.{SpecificationWithJUnit}

class SpecificationBase extends SpecificationWithJUnit  with xUnit  {
  def AreEqual(actual: Line3D, expected: Line3D) {
    AreEqual(actual.A, expected.A)
    AreEqual(actual.B, expected.B)
  }

  def AreEqual(actual: ProjectedLine, expected: ProjectedLine) {
    AreEqual(actual.A, expected.A)
  }

  def AreEqual(actual: Point2D, expected: Point2D) {
    actual.x must_== expected.x
    actual.y must_== expected.y
  }  

  def AreEqual(actual: Point3D, expected: Point3D) {
    actual.x must_== expected.x
    actual.y must_== expected.y
    actual.z must_== expected.z
  }

  def AreEqual(actual: Line2D, expected: Line2D) {
    OneEndOfTheLineMustBe(actual, expected.A)
    OneEndOfTheLineMustBe(actual, expected.B)
  }

  def AreEqual(actual: Double, expected: Double) {
    assertTrue(Math.abs(expected - actual) < 0.005)
  }

  def OneEndOfTheLineMustBe(line: Line2D, end: Point2D) {
    Console.out.println(line.toString)
    assertTrue(line.A == end || line.B == end)
  }

  def ContainsOneLineWhoseStartEndAre(lines: List[ProjectedLine], endPoint1: Point2D, endPoint2: Point2D) {
    lines.foreach(l => {
      Console.out.println(l.toString)
      if((l.A == endPoint1 && l.B == endPoint2) || (l.A == endPoint2 && l.B == endPoint1))
        return
    })
    assertTrue(false)
  }
}