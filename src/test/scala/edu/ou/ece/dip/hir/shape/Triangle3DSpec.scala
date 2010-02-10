package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Triangle3DSpec extends SpecificationBase {
  "Should tell if closer than a point" in {
    val triangle = new Triangle3D(new Point3D(0, 0, 5),
      new Point3D(2.5, 4.5, 5),
      new Point3D(4.5, 0, 5)

      )

    triangle.isNotBehind(new Point3D(0, 0, 10)) must_== true
    triangle.isNotBehind(new Point3D(0, 0, 1)) must_== false
  }

  "Should tell if closer than a point positive z" in {
    val triangle = new Triangle3D(new Point3D(0, 0, 5),
      new Point3D(4.5, 0, 5),
      new Point3D(2.5, 4.5, 5)
      )

    triangle.isNotBehind(new Point3D(0, 0, 10)) must_== true
    triangle.isNotBehind(new Point3D(0, 0, 1)) must_== false
  }

  "" in {
    val A = new Point3D(2.5, 5, 3.5)
    val B = new Point3D(0, 0, 1)
    val C = new Point3D(5, 0, 1)
    val D = new Point3D(2.5, 2, 6)

    val ABC = new Triangle3D(A, B, C)

    ABC.isCloser(D) must_== true
  }

}