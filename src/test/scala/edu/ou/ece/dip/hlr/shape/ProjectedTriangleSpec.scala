package edu.ou.ece.dip.hlr.shape

import edu.ou.ece.dip.hlr.SpecificationBase
import objectmother.{ProjectedLineMother, ProjectedTriangleMother}
import edu.ou.ece.dip.hlr.utils.{MatlabInterop, CameraUtils}

class ProjectedTriangleSpec extends SpecificationBase {
  "A triangle" should {
    val triangle = ProjectedTriangleMother.create(Point2D(0, 0),
      Point2D(2.5, 5),
      Point2D(5, 0))


    "tell if a line has any intersection with it" >> {

      "Given a line goes through the triangle in the middle, there should be two intersections" >> {
        val line = ProjectedLineMother.create(new Point2D(0, 3), new Point2D(5, 3))
        val intersections = triangle.intersect(line)

        intersections.size must_== 2
        intersections.toList(0).x must_== 1.5
        intersections.toList(0).y must_== 3
        intersections.toList(1).x must_== 3.5
        intersections.toList(1).y must_== 3
      }

      "Given a line inside the triangle, there is no intersection" >> {
        val projectedLine = ProjectedLineMother.create(Point2D(2.5, 1), Point2D(4, 1))
        val intersections = triangle.intersect(projectedLine)

        intersections.size must_== 0
      }
    }

    "cover a line" >> {
      val triangle = CameraUtils.projectTriangle(Triangle3D(Point3D(0, 0, 3),
        Point3D(5, 0, 3),
        Point3D(2.5, 5, 3)), 0.5)

      "Given a line closer to the camera than the triangle" in {
        val line = CameraUtils.project(Point3D(0, 0, 2) to Point3D(5, 5, 2), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
      }

      "Given a line doesn't intersect with the triangle, the line should be left untouched" in {
        val line = CameraUtils.project(Point3D(10, 10, 2) to Point3D(15, 15, 2), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }

      "Given" +
              "# line is further to the camera than the triangle" +
              "# line intersects with one vertex of triangle " +
              "# one end of the line is inside the projected triangle " +
              "# one end of the line is outside of the projected triangle" +
              "Then the line is partialy covered" >> {
        val line = CameraUtils.project(Point3D(2.5, 1, 4) to Point3D(15, 1, 6), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        OneEndOfTheLineMustBe(linesLeft(0), line.B)
        OneEndOfTheLineMustBe(linesLeft(0), Point2D(0.78125000, 0.1041666666))
      }

      "Given " +
              "# line is further to the camera than the triangle" +
              "# line intersects with two vertex of triangle " +
              "# the two ends of the line is NOT on the vertices" +
              "Then the part of the line covered by the triangle should be invisible and two parts left are still visible" >> {
        val line = CameraUtils.project(Point3D(-1, 1, 4) to Point3D(15, 1, 6), 0.5)

        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 2

        ContainsOneLineWhoseStartEndAre(linesLeft, Point2D(0.0597014925, 0.1194029850), line.A)
        ContainsOneLineWhoseStartEndAre(linesLeft, Point2D(0.78461538, 0.097435897), line.B)
      }

      "Given " +
              "# line is on the triangle" +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is inside and on the triangle" +
              "Then the line should be entirely visible" >> {

        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(2.5, 2.5, 3), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is inside the triangle and further to the camera" +
              "Then the entire line should be invisible" >> {
        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(2.5, 2.5, 5), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 0
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is outside the triangle and further to the camera" +
              "Then the line should be visible" >> {
        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(2.5, 20, 5), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }


      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is outside the triangle and closer to the camera" +
              "Then the line should be visible" >> {
        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(2.5, 20, 1), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# and intersects with one vertex and intersection is on the vertex" +
              "Then the line is visible" in {

        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(2.5, -20, 3), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# and intersects with one vertex and intersection is behind the vertex" +
              "Then the line is invisible" in {
        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(2.5, -20, 3.5), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        ContainsOneLineWhoseStartEndAre(linesLeft, Point2D(0.4032258064, 0.0), line.B)
      }

      "Given a line lies on one vertex of the triangle" +
              "Then the line should be visible" >> {
        val line = CameraUtils.project(Point3D(2.5, 5, 3) to Point3D(5, 0, 3), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        ContainsOneLineWhoseStartEndAre(List(linesLeft(0)), line.A, line.B)
      }
    }

    "tell if a point is inside it" >> {
      "Given a point duplicated with one corner of the triangle" >> {
        triangle.isInside(Point2D(0, 0)) mustBe true
      }

      "Given a point on the extension of a vertex of a triangle" +
              "The point should not be inside" >> {
        triangle.isInside(Point2D(10, 0)) mustBe false
      }

      "Given a point on one vertex the triangle" +
              "The point should be inside" >> {
        triangle.isInside(Point2D(3, 0)) mustBe true
      }

      "Given a set of point purly inside the triangle" >> {
        triangle.isInside(new Point2D(2, 1)) mustBe true
        triangle.isInside(new Point2D(2.5, 4)) mustBe true
      }

      "Given a set of point purly outside the triangle" >> {
        triangle.isInside(new Point2D(-1, -1)) mustBe false
        triangle.isInside(new Point2D(2, 10)) mustBe false
      }
    }

    "tell if a line is inside it" >> {
      "Given a line outside" +
              "Then the line should be outside" >> {
        triangle.isInside(new Line2D(new Point2D(-1, -1), new Point2D(2, 1))) mustBe false
      }

      "Given a line inside" +
              "Then the line should be outside" >> {
        triangle.isInside(new Line2D(new Point2D(2.5, 4), new Point2D(2, 1))) mustBe true
      }
    }
  }
}