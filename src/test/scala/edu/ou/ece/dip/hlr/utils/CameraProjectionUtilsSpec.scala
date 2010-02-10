package edu.ou.ece.dip.hlr.utils

import org.specs._
import edu.ou.ece.dip.hlr.shape.{Point2D, Line3D, Point3D}

class CameraProjectionUtilsSpec extends Specification {
 
  "Camera projection" should {
    "project integer point" in {
      val expected = CameraProjectionUtils.projectPoint(new Point3D(1, 3, 4), 2.0)
        expected.x must_== -1
        expected.y must_== -3
    }

    "project Double point" in {
      val expected = CameraProjectionUtils.projectPoint(new Point3D(1, 3, 6), 2.0)
        expected.x must_== -0.5
        expected.y must_== -1.5
    }

    "project line" in {
      
      val startPoint = new Point3D(1, 3, 6)
      val endPoint = new Point3D(5, 4, 4)

      val line = CameraProjectionUtils.projectLine(new Line3D(startPoint, endPoint), 2.0)

      line.start.x must_== -0.5
      line.start.y must_== -1.5
      line.end.x must_== -5
      line.end.y must_== -4
    }

    "project triangle" in {
      val one: Point2D = CameraProjectionUtils.projectPoint(new Point3D(1, 1, 1), 0.25)
      val five: Point2D = CameraProjectionUtils.projectPoint(new Point3D(5, 5, 5), 0.25)
      val three: Point2D = CameraProjectionUtils.projectPoint(new Point3D(3, 3, 3), 0.25)

      5 must_== 5
    }

  }

}

