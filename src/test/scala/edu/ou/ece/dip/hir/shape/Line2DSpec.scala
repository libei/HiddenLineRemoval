package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Line2DSpec extends SpecificationBase {
  "A two-dimentional line" should {
    "tell its length" >> {
      "Given" >> {
        val line: Line2D = new Line2D(new Point2D(0, 0), new Point2D(2, 2))
        AreEqual(line.length, 2.828427)

      }

      "Given two point at the same position, the length should be zero" >> {
        val line: Line2D = new Line2D(new Point2D(0, 0), new Point2D(0, 0))
        line.length must_== 0
      }
    }

    "tell if it intersects with another line" >> {
      "Given a line with zero length, it can not intersect with another lines" >> {
        val A = new Line2D(new Point2D(0, 0), new Point2D(0, 0))
        val B = new Line2D(new Point2D(-2, 0), new Point2D(2, 0))
        A.intersect(B) must_== None
      }
    }

    "tell if its extention intersects with another line" >> {

      "Given a line chuizhi to X axis" +
              "and another line is not parallel" +
              "Then two lines should intersect on the extension" >> {
        val A = new Line2D(new Point2D(0, 0), new Point2D(0, 2))
        val B = new Line2D(new Point2D(1, 1), new Point2D(3, 1))
        A.intersectExtension(B).get must_== new Point2D(0, 1)
      }

      "Given two lines which are not parallel or chuizhi to any aixs, they should intersect on the extension" >> {
        val A = new Line2D(new Point2D(0, 0), new Point2D(1, 1))
        val B = new Line2D(new Point2D(2, 1), new Point2D(3, 1))
        A.intersectExtension(B).get must_== new Point2D(1, 1)
      }

      "Given two parallel lines, there is no intersection" in {
        val A = new Line2D(new Point2D(0, 1), new Point2D(1, 1))
        val B = new Line2D(new Point2D(0, 2), new Point2D(1, 2))
        A.intersectExtension(B) must_== None
      }
    }
  }
}