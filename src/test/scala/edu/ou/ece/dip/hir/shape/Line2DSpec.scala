package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Line2DSpec extends SpecificationBase {
  "Should tell length" in {
    val line: Line2D = new Line2D(new Point2D(0, 0), new Point2D(2, 2))
    AreEqual(line.length, 2.828427)
  }

  "If the start point is the same as ending point, the length should be zero" in {
    val line: Line2D = new Line2D(new Point2D(0, 0), new Point2D(0, 0))
    line.length must_== 0
  }

  "If the line has zero length then it should not intersect with another line" in {
    val A = new Line2D(new Point2D(0, 0), new Point2D(0, 0))
    val B = new Line2D(new Point2D(-2, 0), new Point2D(2, 0))
    A.intersect(B) must_== None
  }

  "Can intersect on extension line when one line is chuizhi to one x axis" in {
    val A = new Line2D(new Point2D(0, 0), new Point2D(0, 2))
    val B = new Line2D(new Point2D(1, 1), new Point2D(3, 1))
    A.intersectExtension(B).get must_== new Point2D(0, 1)
  }

  "Can intersect on extension line when one line is not chuizhi to one x axis" in {
    val A = new Line2D(new Point2D(0, 0), new Point2D(1, 1))
    val B = new Line2D(new Point2D(2, 1), new Point2D(3, 1))
    A.intersectExtension(B).get must_== new Point2D(1, 1)
  }

  "Should have no intersection if two lines are parallel" in {
    val A = new Line2D(new Point2D(0, 1), new Point2D(1, 1))
    val B = new Line2D(new Point2D(0, 2), new Point2D(1, 2))
    A.intersectExtension(B) must_== None
  }

  "" in {
    val A = new Line2D(new Point2D(0.3, 0.05), new Point2D(0.30000004, 0.07142857))
    val B = new Line2D(new Point2D(0.3, 0.3), new Point2D(0.3, 0.1))
    A.intersect(B) must_== None
    B.intersect(A) must_== None
  }


}