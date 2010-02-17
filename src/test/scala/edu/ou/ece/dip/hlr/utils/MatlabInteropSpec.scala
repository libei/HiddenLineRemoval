package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.SpecificationBase
import edu.ou.ece.dip.hlr.shape.{Trapezoid3D, Point3D, Triangle3D}

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
        t.getVertexAB.name must_== "0"
        t.getVertexBC must_== B.to(C)
        t.getVertexBC.name must_== "1"
        t.getVertexCA must_== C.to(A)
        t.getVertexCA.name must_== "2"
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
      
      lines(0).s_x must_== line_start_x(0)
      lines(0).s_y must_== line_start_y(0)
      lines(0).s_z must_== line_start_z(0)
      lines(0).e_x must_== line_end_x(0)
      lines(0).e_y must_== line_end_y(0)
      lines(0).e_z must_== line_end_z(0)
      lines(0).index must_== index(0)

      lines(1).s_x must_== line_start_x(1)
      lines(1).s_y must_== line_start_y(1)
      lines(1).s_z must_== line_start_z(1)
      lines(1).e_x must_== line_end_x(1)
      lines(1).e_y must_== line_end_y(1)
      lines(1).e_z must_== line_end_z(1)
      lines(1).index must_== index(1)      



    }


  }
}