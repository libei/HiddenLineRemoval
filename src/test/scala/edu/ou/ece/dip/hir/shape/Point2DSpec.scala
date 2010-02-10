package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Point2DSpec extends SpecificationBase {
  "Should get the distance to another point" >> {
    val from = new Point2D(2, 2)
    "Given a point at the same position" >> {
      from.distanceTo(from) must_== 0
    }

    "Given a point not at the same position" >> {
      from.distanceTo(Point2D(0, 2)) must_== 2
    }
  }
}