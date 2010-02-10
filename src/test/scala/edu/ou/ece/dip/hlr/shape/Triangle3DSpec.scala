package edu.ou.ece.dip.hlr.shape

import edu.ou.ece.dip.hlr.SpecificationBase

class Triangle3DSpec extends SpecificationBase {
  "A 3-dimentional Triangle" should {
    val triangle = new Triangle3D(Point3D(0, 0, 5),
      Point3D(2.5, 4.5, 5),
      Point3D(4.5, 0, 5))

    "tell if it is closer to the camera than a point " >> {

      "Given a point closer to the camera, it should return false" >> {
        triangle isCloserThan Point3D(0, 0, 1) mustBe false
        triangle |* Point3D(0, 0, 1) mustBe false
      }

      "Given a point further to the camera, it should return true" >> {
        triangle isCloserThan Point3D(0, 0, 6) mustBe true
        triangle |* Point3D(0, 0, 6) mustBe true
      }

      "Given a point with the same distance to the camera, it should return false" >> {
        triangle isCloserThan Point3D(0, 0, 5) mustBe false
        triangle |* Point3D(0, 0, 5) mustBe false
      }
    }

    "tell if it is further to the camera than a point " >> {
      "Given a point closer to the camera, it should return true" >> {
        triangle isBehind Point3D(0, 0, 1) mustBe true
        triangle *| Point3D(0, 0, 1) mustBe true
      }

      "Given a point further to the camera, it should return false" >> {
        triangle isBehind Point3D(0, 0, 6) mustBe false
        triangle *| Point3D(0, 0, 6) mustBe false
      }

      "Given a point with the same distance to the camera, it should return false" >> {
        triangle isBehind Point3D(0, 0, 5) mustBe false
        triangle *| Point3D(0, 0, 5) mustBe false
      }
    }

    "tell if it on the some plane surface with a point" >> {
      "Given a point on the plane surface of the triangle, it should return true" >> {
        triangle isEqual Point3D(0, 0, 5) mustBe true
        triangle |*| Point3D(0, 0, 5) mustBe true
      }

      "Given a point closer to the camera, it should return false" >> {
        triangle isEqual Point3D(0, 0, 2) mustBe false
        triangle |*| Point3D(0, 0, 2) mustBe false
      }

      "Given a point further to the camera, it should return false" >> {
        triangle isEqual Point3D(0, 0, 6) mustBe false
        triangle |*| Point3D(0, 0, 6) mustBe false
      }
    }
  }
}