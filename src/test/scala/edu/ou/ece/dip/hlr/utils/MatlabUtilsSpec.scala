package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.SpecificationBase
import edu.ou.ece.dip.hlr.shape.{Trapezoid3D, Point3D, Triangle3D}

class MatlabUtilsSpec extends SpecificationBase {
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

    "extract one triangle from the input containing one triangle" >> {


      val index = Array(0, 1, 2)
      val shapeId = Array(3, 3, 3)

      val x_list = Array(a_x, b_x, c_x)
      val y_list = Array(a_y, b_y, c_y)
      val z_list = Array(a_z, b_z, c_z)

      val actual = MatlabUtils.parseShapes(x_list, y_list, z_list, index, shapeId)

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

      val actual = MatlabUtils.parseShapes(x_list, y_list, z_list, index, shapeId)

      actual.length must_== 1

      val t = actual(0).asInstanceOf[Trapezoid3D]

      t.cornerA must_== A
      t.cornerB must_== B
      t.cornerC must_== C
      t.cornerD must_== D
    }

  }
}