package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase

class Trapezoid3DSpec extends SpecificationBase {
  "Should getComposingTriangles into 2 triangles" in {
    val pointA: Point3D = new Point3D(0, 0, 10)
    val pointB: Point3D = new Point3D(10, 0, 10)
    val pointC: Point3D = new Point3D(10, 10, 10)
    val pointD: Point3D = new Point3D(0, 10, 10)

    val t = new Trapezoid3D(pointA, pointB, pointC, pointD)
    val triangles = t.getComposingTriangles
    triangles.length must_== 2
    triangles(0).cornerA must_== pointA
    triangles(0).cornerB must_== pointB
    triangles(0).cornerC must_== pointD

    triangles(1).cornerA must_== pointB
    triangles(1).cornerB must_== pointC
    triangles(1).cornerC must_== pointD
  }

  "Should get composing vertices" in {
    val pointA: Point3D = new Point3D(0, 0, 10)
    val pointB: Point3D = new Point3D(10, 0, 10)
    val pointC: Point3D = new Point3D(10, 10, 10)
    val pointD: Point3D = new Point3D(0, 10, 10)

    val t = new Trapezoid3D(pointA, pointB, pointC, pointD)

    val vertices = t.getComposingVertices
    vertices.length must_== 4

  }
}