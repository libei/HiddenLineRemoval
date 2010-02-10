package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Triangle3DSpec extends SpecificationBase {
  "A 3-dimentional Triangle" should {
    val triangle = new Triangle3D(new Point3D(0, 0, 5),
      new Point3D(2.5, 4.5, 5),
      new Point3D(4.5, 0, 5))

    "tell if it is closer to the camera than a point " >> {

      "Given a point closer to the camera, it should return false" >> {
        triangle.isCloserThan(Point3D(0, 0, 1)) must_== false
      }

      "Given a point further to the camera, it should return true" >> {
        triangle.isCloserThan(Point3D(0, 0, 6)) must_== true
      }

      "Given a point with the same distance to the camera, it should return false" >> {
        triangle.isCloserThan(Point3D(0, 0, 5)) must_== false
      }
    }

    //    "tell if it is further to the camera than a point " >> {
    //      "Given a point closer to the camera, it should return false" >> {
    //        triangle.isBehind(Point3D(0, 0, 1)) must_== false
    //      }
    //
    //      "Given a point further to the camera, it should return true" >> {
    //        triangle.isCloserThan(Point3D(0, 0, 6)) must_== true
    //      }
    //
    //      "Given a point with the same distance to the camera, it should return false" >> {
    //        triangle.isCloserThan(Point3D(0, 0, 5)) must_== false
    //      }
    //    }

    "pick up a right crossproduct ??" in {
      val triangle = new Triangle3D(Point3D(0, 0, 5),
        Point3D(4.5, 0, 5),
        Point3D(2.5, 4.5, 5))

      triangle.isNotBehind(Point3D(0, 0, 10)) must_== true
      triangle.isNotBehind(Point3D(0, 0, 1)) must_== false
    }

  }
}