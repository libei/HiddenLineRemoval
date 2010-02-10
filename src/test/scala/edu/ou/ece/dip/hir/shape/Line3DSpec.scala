package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Line3DSpec extends SpecificationBase {
  "A three-dimentional line" should {
    "Get intersection with another line on extension" in {
      val A: Line3D = new Line3D(new Point3D(-1, 1, 5), new Point3D(10, 1, 5))
      val B: Line3D = new Line3D(new Point3D(0, 1, 0), new Point3D(0, 1, 10))
      val intersection: Option[Point3D] = A.intersectExtension(B)
      AreEqual(intersection.get.x, 0)
      AreEqual(intersection.get.y, 1)
      AreEqual(intersection.get.z, 5)
    }
  }
}