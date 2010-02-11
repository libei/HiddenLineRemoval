package edu.ou.ece.dip.hlr.shape

import collection.mutable.{ListBuffer}
import edu.ou.ece.dip.hlr.utils.{LineUtils, FloatUtil, CameraUtils}

class ProjectedTriangle(AB: ProjectedLine, BC: ProjectedLine, CA: ProjectedLine, val originalTriangle: Triangle3D) {
  private val _vertices = new ListBuffer[ProjectedLine]
  _vertices.append(AB)
  _vertices.append(BC)
  _vertices.append(CA)
  val vertices = _vertices.toList

  val A = AB.A
  val B = AB.B
  val C = BC.B

  private def sign(x: Double): Double = {
    if (FloatUtil.Equals(x, 0))
      return 0
    Math.signum(x)
  }

  def isInside(point: Point2D): Boolean = {
    if ((point == A) || (point == B) || (point == C))
      return true

    val vectorAT: Vector = new Vector(new Line2D(A, point))
    val vectorAB: Vector = new Vector(AB)
    val productABAT: Vector = vectorAB.crossProduct(vectorAT)
    val signABAT = sign(productABAT.zComponent) //todo this way to tell direction of a vector might be wrong

    val vectorBT: Vector = new Vector(new Line2D(B, point))
    val vectorBC: Vector = new Vector(BC)
    val productBCBT: Vector = vectorBC.crossProduct(vectorBT)
    val signBCBT = sign(productBCBT.zComponent)

    val vectorCT: Vector = new Vector(new Line2D(C, point))
    val vectorCA: Vector = new Vector(CA)
    val productCACT: Vector = vectorCA.crossProduct(vectorCT)
    val signCACT = sign(productCACT.zComponent)

    if (signABAT == 0 && (signBCBT == signCACT))
      return true
    if (signBCBT == 0 && (signABAT == signCACT))
      return true
    if (signCACT == 0 && (signABAT == signBCBT))
      return true

    (signABAT == signBCBT) && (signBCBT == signCACT)

  }

  def isInside(line: Line2D): Boolean = {
    isInside(line.A) && isInside(line.B)
  }

  def intersect(line: ProjectedLine): Set[Point2D] = {
    if (isInside(line))
      Set[Point2D]()

    var intersections = Set[Point2D]()
    vertices.foreach(vertex => {
      val intersectionPoint = vertex.intersect(line)
      if (!intersectionPoint.isEmpty) {
        intersections = intersections + intersectionPoint.get
      }
    })
    intersections
  }

  def cover(projectedLine: ProjectedLine): List[ProjectedLine] = {
    LineUtils.removeDuplicated(LineUtils.removeZeroLength(_cover(projectedLine)))
  }

  private def oneEndIsOneThePlateAnotherIsCovered(start: Point2D, end: Point2D, startOriginal: Point3D, endOriginal: Point3D): Boolean = {
    val isStartPointCovered = (originalTriangle |* startOriginal) && isInside(start)
    val isStartPointOnThePlate = (originalTriangle |*| startOriginal) && isInside(start)
    val isStartPointUnCovered = (originalTriangle *| startOriginal) || !isInside(start)

    val isEndPointCovered = (originalTriangle |* endOriginal) && isInside(end)
    val isEndPointOnThePlate = (originalTriangle |*| endOriginal) && isInside(end)
    val isEndPointUnCovered = (originalTriangle |* endOriginal) || !isInside(end)

    (isStartPointCovered && isEndPointOnThePlate) || (isStartPointOnThePlate && isEndPointCovered)
  }

  private def bothCovered(start2D: Point2D, start: Point3D, end2D: Point2D, end: Point3D): Boolean = {
    !isPointVisible(start2D, start) && !isPointVisible(end2D, end)
  }
  
  private def _cover(projectedLine: ProjectedLine): List[ProjectedLine] = {
    if(FloatUtil.Equals(projectedLine.length, 0))
      return List()
    
    val original = List(projectedLine)

    var intersections = intersect(projectedLine)
    if (intersections.size == 0) {
      val A3D = projectedLine.getOriginalPoint(projectedLine.A).get
      val B3D = projectedLine.getOriginalPoint(projectedLine.B).get

      val ACovered = (originalTriangle |* A3D) && isInside(projectedLine.A)
      val BCovered = (originalTriangle |* B3D) && isInside(projectedLine.B)
      if(ACovered || BCovered)
        return List()
    }
    if (intersections.size == 1) {
      val i = intersections.toList(0)

      val theOnlyIntersectionIsAtTheEndOfTheLine = projectedLine.A == i || projectedLine.B == i
      if (theOnlyIntersectionIsAtTheEndOfTheLine) {
        val startOriginal = projectedLine.getOriginalPoint(projectedLine.A).get
        val endOriginal = projectedLine.getOriginalPoint(projectedLine.B).get

        if (oneEndIsOneThePlateAnotherIsCovered(projectedLine.A, projectedLine.B, startOriginal, endOriginal))
          return List()

        if (bothCovered(projectedLine.A, startOriginal, projectedLine.B, endOriginal))
          return List()

        return original
      }

      val (endpointOutsideTriangle, originalEndpointInsideTriangle) =
      if (isInside(projectedLine.A))
        (projectedLine.B, projectedLine.getOriginalPoint(projectedLine.A))
      else
        (projectedLine.A, projectedLine.getOriginalPoint(projectedLine.B))

      if (originalTriangle.isCloserThan(originalEndpointInsideTriangle.get))
        return List(projectedLine.subLine(endpointOutsideTriangle, i))
      else
        return original
    }

    if (intersections.size == 2) {
      val intersectionOne = intersections.toList(0)
      val intersectionTwo = intersections.toList(1)
      val originalPoint1 = projectedLine.getOriginalPoint(intersectionOne)
      val originalPoint2 = projectedLine.getOriginalPoint(intersectionTwo)

      val isStartVisible = isPointVisible(intersectionOne, originalPoint1.get)
      val isEndVisible = isPointVisible(intersectionTwo, originalPoint2.get)

      if (isStartVisible && isEndVisible)
        return original

      if (intersectionOne.distanceTo(projectedLine.A) < intersectionTwo.distanceTo(projectedLine.A)) {
        return List(projectedLine.subLine(projectedLine.A, intersectionOne),
          projectedLine.subLine(intersectionTwo, projectedLine.B))
      } else {
        return List(projectedLine.subLine(projectedLine.A, intersectionTwo),
          projectedLine.subLine(intersectionOne, projectedLine.B))
      }
    }
    original
  }

  def isPointVisible(projectPoint: Point2D, originalPoint: Point3D): Boolean = {
    if (!isInside(projectPoint))
      return true
    
    val behind: Boolean = originalTriangle *| originalPoint
    val equal: Boolean = originalTriangle |*| originalPoint
    behind || equal
  }
}