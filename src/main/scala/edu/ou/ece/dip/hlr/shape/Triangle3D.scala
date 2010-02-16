package edu.ou.ece.dip.hlr.shape

import edu.ou.ece.dip.hlr.utils.FloatUtil

object Triangle3D {
  def apply(A: Point3D, B: Point3D, C: Point3D): Triangle3D = {
    new Triangle3D(A, B, C)
  }
}

class Triangle3D(val cornerA: Point3D, val cornerB: Point3D, val cornerC: Point3D) extends Decomposable {
  val lineAB = new Line3D(cornerA, cornerB)
  val lineBC = new Line3D(cornerB, cornerC)
  val lineCA = new Line3D(cornerC, cornerA)
  
  val vertices = List(lineAB, lineBC, lineCA)
  def getVertexAB = lineAB
  def getVertexBC = lineBC
  def getVertexCA = lineCA

  def triangles = List(this) 

  def isBehind(point: Point3D): Boolean = {
    FloatUtil.LessThan(depth(point), 0)
  }

  def isEqual(point: Point3D): Boolean = {
    FloatUtil.Equals(depth(point), 0)
  }

  def isCloserThan(point: Point3D): Boolean = {
    FloatUtil.GreaterThan(depth(point), 0)
  }

  def *| (point: Point3D): Boolean = {
    FloatUtil.LessThan(depth(point), 0)
  }

  def |* (point: Point3D): Boolean = {
    FloatUtil.GreaterThan(depth(point), 0)
  }

  def |*| (point: Point3D): Boolean = {
    FloatUtil.Equals(depth(point), 0)
  }

  private def depth(point: Point3D): Double = {
    val lineAC = new Line3D(cornerA, cornerC)
    val vectorALine = new Vector(new Line3D(cornerA, point))
    val vectorAB = new Vector(lineAB)
    val vectorAC = new Vector(lineAC)


    val crossProductABAC = vectorAB.crossProduct(vectorAC)
    val crossProductACAB = vectorAC.crossProduct(vectorAB)
    val vectorPointToOriginal = new Vector(-cornerA.x, -cornerA.y, 0.5 - cornerA.z)
    val positiveZCrossProduct = if (crossProductABAC.dotProduct(vectorPointToOriginal) < 0) crossProductABAC else crossProductACAB
    positiveZCrossProduct.dotProduct(vectorALine)
  }
  override def toString = "%(" + cornerA.toString + " " + cornerB.toString + " " + cornerC.toString + ")"
}