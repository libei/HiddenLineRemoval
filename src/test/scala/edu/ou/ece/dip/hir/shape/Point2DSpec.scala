package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Point2DSpec extends SpecificationBase {
  "Should get the distance to another point" in {
    val from = new Point2D(2, 2)
    val to = new Point2D(0, 2)

    from.distanceTo(to) must_== 2
  }
}