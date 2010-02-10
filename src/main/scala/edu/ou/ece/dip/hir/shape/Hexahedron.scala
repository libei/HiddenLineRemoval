package edu.ou.ece.dip.hir.shape

import collection.mutable.{HashSet, ListBuffer}

class

Hexahedron(val cornerA: Point3D, val cornerB: Point3D, val cornerC: Point3D, val cornerD: Point3D,
                 val cornerE: Point3D, val cornerF: Point3D, val cornerG: Point3D, val cornerH: Point3D) {

  val faceABCD: Trapezoid3D = new Trapezoid3D(cornerA, cornerB, cornerC, cornerD)
  val faceEFGH: Trapezoid3D = new Trapezoid3D(cornerE, cornerF, cornerG, cornerH)
  val faceEADH: Trapezoid3D = new Trapezoid3D(cornerE, cornerA, cornerD, cornerH)
  val faceBFGC: Trapezoid3D = new Trapezoid3D(cornerB, cornerF, cornerG, cornerC)
  val faceABFE: Trapezoid3D = new Trapezoid3D(cornerA, cornerB, cornerF, cornerE)
  val faceDCGH: Trapezoid3D = new Trapezoid3D(cornerD, cornerC, cornerG, cornerH)
  val faces = List(faceABCD, faceABFE, faceBFGC, faceDCGH, faceEADH, faceEFGH)

  private val composingTrianglesBuffer = new ListBuffer[Triangle3D]
  faces.foreach(f => {
    composingTrianglesBuffer.appendAll(f.getComposingTriangles)
  })
  val composingTriangles = composingTrianglesBuffer.toList


  private val set = new HashSet[Line3D]
  faces.foreach(f => {
    f.getComposingVertices.foreach(v => {
      set + v
    })
  })
  val composingVertices = set.toList


}