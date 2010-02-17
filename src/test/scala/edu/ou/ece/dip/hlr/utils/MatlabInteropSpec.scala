package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.SpecificationBase
import objectmother.{Line3DMother, ProjectedLineMother}
import edu.ou.ece.dip.hlr.shape._

class MatlabInteropSpec extends SpecificationBase {
  
  "remove hidden line" should {
    val a_x = 0.0
    val a_y = 0.0
    val a_z = 1.0

    val b_x = 5.0
    val b_y = 0.0
    val b_z = 1.0

    val c_x = 5.0
    val c_y = 10.0
    val c_z = 1.0

    val d_x = 0.0
    val d_y = 10.0
    val d_z = 1.0


    val A = Point3D(a_x, a_y, a_z)
    val B = Point3D(b_x, b_y, b_z)
    val C = Point3D(c_x, c_y, c_z)
    val D = Point3D(d_x, d_y, d_z)

    "parse shapes" >> {

      "extract one triangle from the input containing one triangle" >> {


        val index = Array(0, 1, 2)
        val shapeId = Array(3, 3, 3)

        val x_list = Array(a_x, b_x, c_x)
        val y_list = Array(a_y, b_y, c_y)
        val z_list = Array(a_z, b_z, c_z)

        val actual = MatlabInterop.parseShapes(x_list, y_list, z_list, index, shapeId)

        actual.length must_== 1

        val t = actual(0).asInstanceOf[Triangle3D]

        t.getVertexAB must_== A.to(B)
        t.getVertexBC must_== B.to(C)
        t.getVertexCA must_== C.to(A)
      }

      "extract one trapezoid from the input containing one trapezoid" >> {
        val index = Array(0, 1, 2, 3)
        val shapeId = Array(4, 4, 4, 4)

        val x_list = Array(a_x, b_x, c_x, d_x)
        val y_list = Array(a_y, b_y, c_y, d_y)
        val z_list = Array(a_z, b_z, c_z, d_z)

        val actual = MatlabInterop.parseShapes(x_list, y_list, z_list, index, shapeId)

        actual.length must_== 1

        val t = actual(0).asInstanceOf[Trapezoid3D]

        t.cornerA must_== A
        t.cornerB must_== B
        t.cornerC must_== C
        t.cornerD must_== D
      }
    }

    "parse lines" >> {

      val line_start_x = Array(0.0, 1.0)
      val line_start_y = Array(0.0, 1.0)
      val line_start_z = Array(0.0, 1.0)

      val line_end_x = Array(5.0, 6.0)
      val line_end_y = Array(5.0, 6.0)
      val line_end_z = Array(5.0, 6.0)

      val index = Array(0, 1)

      val lines = MatlabInterop.parseLines(line_start_x, line_start_y, line_start_z, line_end_x, line_end_y, line_end_z, index)

      lines.length must_== 2
      
      lines(0).A.x must_== line_start_x(0)
      lines(0).A.y must_== line_start_y(0)
      lines(0).A.z must_== line_start_z(0)
      lines(0).B.x must_== line_end_x(0)
      lines(0).B.y must_== line_end_y(0)
      lines(0).B.z must_== line_end_z(0)
      lines(0).name must_== index(0).toString

      lines(1).A.x must_== line_start_x(1)
      lines(1).A.y must_== line_start_y(1)
      lines(1).A.z must_== line_start_z(1)
      lines(1).B.x must_== line_end_x(1)
      lines(1).B.y must_== line_end_y(1)
      lines(1).B.z must_== line_end_z(1)
      lines(1).name must_== index(1).toString

    }

    "generate result" >> {
      val out_start_x = new Array[Double](100)
      val out_start_y = new Array[Double](100)
      val out_end_x = new Array[Double](100)
      val out_end_y = new Array[Double](100)
      val index = new Array[Int](100)

      val line1 = ProjectedLineMother.create(Point2D(5.0, 5.0), Point2D(6.0, 6.0), Line3DMother.A)
      val line2 = ProjectedLineMother.create(Point2D(7.0, 7.0), Point2D(8.0, 8.0), Line3DMother.B)
      MatlabInterop.generateRes(List(line1, line2), out_start_x, out_start_y, out_end_x, out_end_y, index)

      out_start_x(0) must_== 5.0
      out_start_y(0) must_== 5.0
      out_end_x(0) must_== 6.0
      out_end_y(0) must_== 6.0
      index(0) must_== Integer.parseInt(Line3DMother.A.name)
      
      out_start_x(1) must_== 7.0
      out_start_y(1) must_== 7.0
      out_end_x(1) must_== 8.0
      out_end_y(1) must_== 8.0
      index(1) must_== Integer.parseInt(Line3DMother.B.name)
    }

  }
}