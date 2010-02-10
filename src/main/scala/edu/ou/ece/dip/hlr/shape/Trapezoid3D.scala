package edu.ou.ece.dip.hlr.shape

class Trapezoid3D(val cornerA: Point3D,val cornerB: Point3D,val cornerC: Point3D,val cornerD: Point3D) {

  def getComposingTriangles(): List[Triangle3D] = {
    List(new Triangle3D(cornerA, cornerB, cornerC), new Triangle3D(cornerA, cornerC, cornerD))
  }

  def getComposingVertices =
    List(new Line3D(cornerA, cornerB), new Line3D(cornerB, cornerC), new Line3D(cornerC, cornerD), new Line3D(cornerD, cornerA))

}
