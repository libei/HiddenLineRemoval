package edu.ou.ece.dip.hir.shape

import org.specs.runner.ScalaTest
import org.specs.Specification

class VectorSpec extends Specification with ScalaTest {
  "Dot product" should {
    "returns zero when the angle is 90" in {
      val vectorA = new Vector(new Line3D(new Point3D(0, 0, 0), new Point3D(1, 0, 0)))
      val vectorB = new Vector(new Line3D(new Point3D(0, 0, 0), new Point3D(0, 1, 0)))

      vectorA.dotProduct(vectorB) must_== 0

    }

    "should do dot product" in {

      val vectorA = new Vector(new Line3D(new Point3D(1, 1, 0), new Point3D(3, 3, 0)))
      val vectorB = new Vector(new Line3D(new Point3D(1, 1, 0), new Point3D(5, 5, 0)))

      vectorA.dotProduct(vectorB) must_== 16
    }

  }

  "should do cross product" in {

    val vectorA = new Vector(1, 2, 3)
    val vectorB = new Vector(4, 5, 6)

    vectorA.crossProduct(vectorB) must_== new Vector(-3, 6, -3)
  }

}