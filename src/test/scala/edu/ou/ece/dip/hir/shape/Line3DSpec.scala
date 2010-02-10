package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Line3DSpec extends SpecificationBase {
  "A three-dimentional line" should {
    "Get intersection with another line on extension" in {
      val AB = Point3D(-1, 1, 5) to Point3D(10, 1, 5)
      val CD = Point3D(0, 1, 0) to Point3D(0, 1, 10)
      val intersection = AB intersectExtension CD
      AreEqual(intersection.get.x, 0)
      AreEqual(intersection.get.y, 1)
      AreEqual(intersection.get.z, 5)
    }
  }

  "Should generate a line with another point" >> {
    val A = Point3D(-1, 1, 5)
    val B = Point3D(10, 1, 5)

    A to B must_== new Line3D(A, B)
  }
}