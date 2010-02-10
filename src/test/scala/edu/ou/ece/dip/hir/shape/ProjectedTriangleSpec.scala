package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase
import objectmother.{ProjectedLineMother, ProjectedTriangleMother}
import edu.ou.ece.dip.hir.utils.{MatlabUtils, CameraProjectionUtils}

class ProjectedTriangleSpec extends SpecificationBase {
  "A triangle" should {
    val triangle = ProjectedTriangleMother.create(Point2D(0, 0),
      Point2D(2.5, 5),
      Point2D(5, 0))
    "tell if a line has any intersection with it" >> {

      "Given a line goes through the triangle in the middle, there should be two intersections" >> {
        val projectedLine = ProjectedLineMother.create(new Point2D(0, 3), new Point2D(5, 3))
        val intersections = triangle.intersect(projectedLine)

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
      val triangle = CameraProjectionUtils.projectTriangle(Triangle3D(Point3D(0, 0, 3),
        Point3D(5, 0, 3),
        Point3D(2.5, 5, 3)), 0.5)
      "Given a line closer to the camera than the triangle" in {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(0, 0, 2), Point3D(5, 5, 2)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
      }

      "Given a line doesn't intersect with the triangle, the line should be left untouched" in {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(10, 10, 2), Point3D(15, 15, 2)), 0.5)
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
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 1, 4), Point3D(15, 1, 6)), 0.5)

        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1

        OneEndOfTheLineMustBe(linesLeft(0), line.end)
        OneEndOfTheLineMustBe(linesLeft(0), Point2D(0.78125, 0.104166))
      }

      "Given " +
              "# line is further to the camera than the triangle" +
              "# line intersects with two vertex of triangle " +
              "# the two ends of the line is NOT on the vertices" +
              "Then the part of the line covered by the triangle should be invisible and two parts left are still visible" >> {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(-1, 1, 4), Point3D(15, 1, 6)), 0.5)

        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 2

        ContainsOneLineWhoseStartEndAre(linesLeft, Point2D(0.059701, 0.119402), line.start)
        ContainsOneLineWhoseStartEndAre(linesLeft, Point2D(0.784615, 0.097435), line.end)
      }

      "Given " +
              "# line is on the triangle" +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is inside and on the triangle" +
              "Then the line should be entirely visible" >> {

        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(2.5, 2.5, 3)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is inside the triangle and further to the camera" +
              "Then the entire line should be invisible" >> {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(2.5, 2.5, 5)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 0
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is outside the triangle and further to the camera" +
              "Then the line should be visible" >> {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(2.5, 20, 5)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }


      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# another end of the line is outside the triangle and closer to the camera" +
              "Then the line should be visible" >> {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(2.5, 20, 1)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }

      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# and intersects with one vertex and intersection is on the vertex" +
              "Then the line is visible" in {

        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(2.5, -20, 3)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 1
        AreEqual(linesLeft(0), line)
      }


      "Given " +
              "# one end of the line is on one corner of the triangle" +
              "# and intersects with one vertex and intersection is behind the vertex" +
              "Then the line is invisible" in {
        val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(2.5, -20, 3.5)), 0.5)
        val linesLeft = triangle.cover(line)
        linesLeft.length must_== 0
      }
    }

    "tell if a point is inside it" >> {
      "Given a point duplicated with one corner of the triangle" >> {
        triangle.isInside(Point2D(0, 0)) must_== true
      }

      "Given a point on the extension of a vertex of a triangle" +
              "The point should not be inside" >> {
        triangle.isInside(Point2D(10, 0)) must_== false
      }

      "Given a point on one vertex the triangle" +
              "The point should be inside" >> {
        triangle.isInside(Point2D(3, 0)) must_== true
      }

      "Given a set of point purly inside the triangle" >> {
        triangle.isInside(new Point2D(2, 1)) must_== true
        triangle.isInside(new Point2D(2.5, 4)) must_== true
      }

      "Given a set of point purly outside the triangle" >> {
        triangle.isInside(new Point2D(-1, -1)) must_== false
        triangle.isInside(new Point2D(2, 10)) must_== false
      }
    }

    "tell if a line is inside it" >> {
      "Given " >> {

      }
    }
  }
  //  "Should tell if a line is inside a triangle" in {
  //
  //    val triangle = ProjectedTriangleMother.create(Point2D(0, 0),
  //      Point2D(2.5, 5),
  //      Point2D(5, 0))
  //
  //    triangle.isInside(new Line2D(new Point2D(-1, -1), new Point2D(2, 1))) must_== false
  //    triangle.isInside(new Line2D(new Point2D(2.5, 4), new Point2D(2, 1))) must_== true
  //  }


  //
  //  "Given a triangle and a line" +
  //          "the line is exactly the same with one vertex of the triangle" in {
  //    val triangle = CameraProjectionUtils.projectTriangle(Triangle3D(Point3D(0, 0, 3),
  //      Point3D(5, 0, 3),
  //      Point3D(2.5, 5, 3)), 0.5)
  //
  //    val line = CameraProjectionUtils.projectLine(Line3D(Point3D(2.5, 5, 3), Point3D(5, 0, 3)), 0.5)
  //    Console.out.println(line.toString)
  //    val linesLeft = triangle.cover(line)
  //    linesLeft.foreach(Console.out.println _)
  //    linesLeft.length must_== 1
  //    ContainsOneLineWhoseStartEndAre(List(linesLeft(0)), line.start, line.end)
  //
  //  }
  //
  //  "" in {
  //    val length = 10
  //    val height = 10
  //    val offset = 2
  //
  //    val A = Point3D(0, 0, 10)
  //    val B = Point3D(length, 0, 10)
  //    val C = Point3D(length, height, 10)
  //    val F = Point3D(length + offset, 0, 20)
  //    val G = Point3D(length + offset, height, 20)
  //
  //    //    val ABC = CameraProjectionUtils.projectTriangle(Triangle3D(A, B, C), 0.5)
  //    //    val FB = CameraProjectionUtils.projectLine(Line3D(F, B), 0.5)
  //    //
  //    //    ABC.cover(FB).size must_== 0
  //
  //    val CFB = CameraProjectionUtils.projectTriangle(Triangle3D(C, F, B), 0.5)
  //    val AB = CameraProjectionUtils.projectLine(Line3D(A, B), 0.5)
  //    Console.out.println(AB.toString)
  //    val linesLeft = CFB.cover(AB)
  //    linesLeft.size must_== 1
  //    ContainsOneLineWhoseStartEndAre(linesLeft.toList, AB.start, AB.end)
  //  }
  //
  //  "1" in {
  //    val A = new Point3D(2, 2, 10)
  //    val B = new Point3D(6, 2, 10)
  //    val C = new Point3D(6, 6, 10)
  //
  //    val E = new Point3D(12, 2, 20)
  //    val F = new Point3D(18, 2, 20)
  //    val G = new Point3D(18, 6, 20)
  //    val H = new Point3D(12, 6, 20)
  //
  //    //    val ABE = CameraProjectionUtils.projectTriangle(Triangle3D(A, B, E), 0.5)
  //    val EH = CameraProjectionUtils.projectLine(Line3D(E, H), 0.5)
  //    //    val HE = CameraProjectionUtils.projectLine(Line3D(H, E), 0.5)
  //    //    val BC = CameraProjectionUtils.projectLine(Line3D(B, C), 0.5)
  //    //
  //    //    val ABF = CameraProjectionUtils.projectTriangle(Triangle3D(A, B, F), 0.5)
  //    //    val AEF = CameraProjectionUtils.projectTriangle(Triangle3D(A, E, F), 0.5)
  //
  //    //    Console.out.println(EH.toString)
  //    val EBG = CameraProjectionUtils.projectTriangle(Triangle3D(E, B, G), 0.5)
  //
  //    var linesLeft = EBG.cover(EH)
  //    //    Console.out.println(linesLeft.toList(0).toString)
  //    //    linesLeft.size must_== 2
  //    Console.out.println(linesLeft.toList(0).toString)
  //    Console.out.println(linesLeft.toList(1).toString)
  //    //    linesLeft = AEF.cover(linesLeft.toList(0))
  //    //    linesLeft.size must_== 1
  //    //    Console.out.println(linesLeft.toList(0).toString)
  //    //    linesLeft = ABE.cover(HE)
  //    //    linesLeft.size must_== 0
  //    //    Console.out.println(linesLeft.toList(0).toString)
  //    //    ContainsOneLineWhoseStartEndAre(linesLeft.toList, BC.start, BC.end)
  //  }

  //  "Should tell if a point is inside" in {
  //    val B = Point3D(6, 2, 10)
  //    val F = Point3D(18, 2 + -10, 20, "F")
  //    val G = Point3D(18, 6 + -10, 20, "G")
  //
  //    val H = Point3D(12, 6 + -10, 20, "H")
  //
  //
  //    val p3d = Point3D(16.000002, -4.000001, 20.0)
  //    val p2d = CameraProjectionUtils.projectPoint(p3d, 0.5)
  //    println(p2d.toString)
  //    val BFG = CameraProjectionUtils.projectTriangle(Triangle3D(B, F, G), 0.5)
  //    BFG.vertices.toList.length must_== 3
  //    println(MatlabUtils.generateMatrixForLineDisplay(BFG.vertices.toList))
  //    BFG.isInside(p2d) must_== true
  //  }

  //  "" in {
  //    val v_3 = new Point3D(2.200000, 0.330000, 10.594000, "v_3")
  //    val v_4 = new Point3D(2.200000, 0.330000, 10.000000, "v_4")
  //    val v_5 = new Point3D(1.639000, 0.440000, 10.594000, "v_5")
  //    val v_6 = new Point3D(1.639000, 0.440000, 10.000000, "v_6")
  //
  //    val t356 = new Triangle3D(v_3, v_5, v_6)
  //    val t364 = new Triangle3D(v_3, v_6, v_4)
  //    val p56 = CameraProjectionUtils.projectLine(Line3D(v_5, v_6), 0.5)
  //    val p46 = CameraProjectionUtils.projectLine(Line3D(v_4, v_6), 0.5)
  //    val p35 = CameraProjectionUtils.projectLine(Line3D(v_3, v_5), 0.5)
  //    val p34 = CameraProjectionUtils.projectLine(Line3D(v_3, v_4), 0.5)
  //    val p364 = CameraProjectionUtils.projectTriangle(t356, 0.5)
  ////    var linesLeft = p364.cover(p56)
  ////    linesLeft.size must_== 1
  ////    ContainsOneLineWhoseStartEndAre(linesLeft.toList, p56.start, p56.end)
  ////
  ////    linesLeft = p364.cover(p46)
  ////    linesLeft.size must_== 1
  ////    ContainsOneLineWhoseStartEndAre(linesLeft.toList, p46.start, p46.end)
  ////
  //    val linesLeft = p364.cover(p35)
  //    linesLeft.size must_== 1
  //    ContainsOneLineWhoseStartEndAre(linesLeft.toList, p35.start, p35.end)
  //
  ////    linesLeft = p364.cover(p34)
  ////    linesLeft.size must_== 1
  ////    ContainsOneLineWhoseStartEndAre(linesLeft.toList, p34.start, p34.end)
  //  }

  "abcd" in {
    val v_1 = new Point3D(5.937052f, 0.889881f, 41.982792f, "v_1")
    val v_2 = new Point3D(10.958142f, 1.917426f, 38.980058f, "v_2")
    val v_3 = new Point3D(5.299163f, 4.127380f, 42.024014f, "v_3")
    val v_4 = new Point3D(10.320253f, 5.154925f, 39.021281f, "v_4")
    val v_5 = new Point3D(2.292208f, 4.717603f, 37.197840f, "v_5")
    val v_6 = new Point3D(7.313297f, 5.745148f, 34.195107f, "v_6")
    val v_7 = new Point3D(0.421323f, 8.423523f, 34.206047f, "v_7")
    val v_8 = new Point3D(4.475388f, 9.253171f, 31.781618f, "v_8")
    val v_9 = new Point3D(-3.359236f, 7.762011f, 27.657927f, "v_9")
    val v_10 = new Point3D(0.694829f, 8.591659f, 25.233498f, "v_10")
    val v_11 = new Point3D(-4.282677f, 3.567147f, 25.809805f, "v_11")
    val v_12 = new Point3D(0.738412f, 4.594692f, 22.807072f, "v_12")
    val v_13 = new Point3D(-5.786556f, 2.857454f, 23.052201f, "v_13")
    val v_14 = new Point3D(-0.765466f, 3.884999f, 20.049467f, "v_14")
    val v_15 = new Point3D(-5.021090f, -1.027545f, 23.002733f, "v_15")
    val v_16 = new Point3D(0.000000f, 0.000000f, 20.000000f, "v_16")

    val t6_8_10 = CameraProjectionUtils.projectTriangle(Triangle3D(v_6, v_8, v_10), 0.5)
    val l87 = CameraProjectionUtils.projectLine(Line3D(v_7, v_8), 0.5)
    //    val t2_4_6 = CameraProjectionUtils.projectTriangle(Triangle3D(v_2, v_4, v_6), 0.5)
    //    val l34 = CameraProjectionUtils.projectLine(Line3D(v_3, v_4), 0.5)

    t6_8_10.cover(l87).length must_== 0
  }

  //
  //  "Given " +
  //          "# two triangles and one line" +
  //          "# and two triangle can entirely cover the line" in {
  //    val A = Point3D(7.5, 5, 3.5)
  //    val B = Point3D(5, 0, 1)
  //    val C = Point3D(10, 0, 1)
  //
  //    val D = Point3D(2.5, 2, 6)
  //
  //    val ABD = Triangle3D(A, B, D)
  //    val ABC = Triangle3D(A, B, C)
  //    val lineCD = Line3D(C, D)
  //
  //    val triangleABD = CameraProjectionUtils.projectTriangle(ABD, 0.5)
  //    val triangleABC = CameraProjectionUtils.projectTriangle(ABC, 0.5)
  //
  //    var line = CameraProjectionUtils.projectLine(lineCD, 0.5)
  //    Console.out.println("Original " + line.toString)
  //
  //    var linesLeft = triangleABC.cover(line)
  //    linesLeft.length must_== 1
  //    Console.out.println("After ABC " + linesLeft(0))
  //    linesLeft = triangleABD.cover(linesLeft(0))
  //    linesLeft.length must_== 0
  //  }

}