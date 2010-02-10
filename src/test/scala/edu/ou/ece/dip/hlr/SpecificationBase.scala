package edu.ou.ece.dip.hlr

import org.specs.Specification
import org.specs.matcher.xUnit
import shape._

class SpecificationBase extends Specification with xUnit  {
  def AreEqual(actual: Line3D, expected: Line3D) {
    AreEqual(actual.start, expected.start)
    AreEqual(actual.end, expected.end)
  }

  def AreEqual(actual: ProjectedLine, expected: ProjectedLine) {
    AreEqual(actual.start, expected.start)
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
    OneEndOfTheLineMustBe(actual, expected.start)
    OneEndOfTheLineMustBe(actual, expected.end)
  }

  def AreEqual(actual: Double, expected: Double) {
    assertTrue(Math.abs(expected - actual) < 0.005)
  }

  def OneEndOfTheLineMustBe(line: Line2D, end: Point2D) {
    Console.out.println(line.toString)
    assertTrue(line.start == end || line.end == end)
  }

  def ContainsOneLineWhoseStartEndAre(lines: List[ProjectedLine], endPoint1: Point2D, endPoint2: Point2D) {
    lines.foreach(l => {
      Console.out.println(l.toString)
      if((l.start == endPoint1 && l.end == endPoint2) || (l.start == endPoint2 && l.end == endPoint1))
        return
    })
    assertTrue(false)
  }
}