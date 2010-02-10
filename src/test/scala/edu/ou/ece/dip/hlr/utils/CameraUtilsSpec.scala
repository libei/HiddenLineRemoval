package edu.ou.ece.dip.hlr.utils

import org.specs._
import edu.ou.ece.dip.hlr.shape.{Point3D}

class CameraUtilsSpec extends Specification {
 
  "Camera projection" should {
    "project point" in {
      val actual = CameraUtils.project(Point3D(1, 3, 4), 2.0)
        actual.x must_== 0.5
        actual.y must_== 1.5
    }

    "project line" in {
      
      val A = Point3D(4, 4, 2)
      val B = Point3D(8, 8, 1)

      val actual = CameraUtils.project(A to B, 2.0)

      actual.start.x must_== 4
      actual.start.y must_== 4
      actual.end.x must_== 16
      actual.end.y must_== 16
    }

    "project triangle" in {
      val one = CameraUtils.project(Point3D(1, 1, 1), 0.25)
      val five = CameraUtils.project(Point3D(5, 5, 5), 0.25)
      val three = CameraUtils.project(Point3D(3, 3, 3), 0.25)

      5 must_== 5
    }

  }

}

