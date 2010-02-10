package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Point2DSpec extends SpecificationBase {
  val A = new Point2D(2, 2)
  "Should get the distance to another point" >> {

    "Given a point at the same position" >> {
      A.distanceTo(A) must_== 0
    }

    "Given a point not at the same position" >> {
      A.distanceTo(Point2D(0, 2)) must_== 2
    }
  }

  "Should generate a line with another point" >> {
    val B = new Point2D(5, 5)
    A to B must_== Line2D(A, B)
  }
}