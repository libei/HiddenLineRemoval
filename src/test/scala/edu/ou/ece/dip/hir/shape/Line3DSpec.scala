package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Line3DSpec extends SpecificationBase {

  "Given a point in the line, we should get coefficient" in {
    val lineA = new Line3D(new Point3D(1, 0, 0), new Point3D(1, 2, 0))
    val lineB = new Line3D(new Point3D(0, 1, 0), new Point3D(2, 1, 0))

    lineA.getCoefficient(new Point3D(1, 1, 0)) must_==0.5
    lineB.getCoefficient(new Point3D(1, 1, 0)) must_==0.5
  }

  "Can get intersection" in {
    val A: Line3D = new Line3D(new Point3D(-1, 1, 5), new Point3D(10, 1, 5))
    val B: Line3D = new Line3D(new Point3D(0, 1, 0), new Point3D(0, 1, 10))
    val intersection: Option[Point3D] = A.intersectExtension(B)
    AreEqual(intersection.get.x, 0)
    AreEqual(intersection.get.y, 1)
    AreEqual(intersection.get.z, 5)
  }
}