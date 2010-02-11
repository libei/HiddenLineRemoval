package edu.ou.ece.dip.hlr.shape

import collection.mutable.{ListBuffer}
import edu.ou.ece.dip.hlr.utils.{LineUtils, FloatUtil, CameraUtils}

class ProjectedTriangle(vertexAB: ProjectedLine, vertexBC: ProjectedLine, vertexCA: ProjectedLine, val originalTriangle: Triangle3D) {
  private val _vertices = new ListBuffer[ProjectedLine]
  _vertices.append(vertexAB)
  _vertices.append(vertexBC)
  _vertices.append(vertexCA)
  val vertices = _vertices.toList

  val A = vertexAB.A
  val B = vertexAB.B
  val C = vertexBC.B

  private def sign(x: Double): Double = {
    if (FloatUtil.Equals(x, 0))
      return 0
    Math.signum(x)
  }

  def isInside(point: Point2D): Boolean = {
    if ((point == A) || (point == B) || (point == C))
      return true

    val vectorAT: Vector = new Vector(new Line2D(A, point))
    val vectorAB: Vector = new Vector(vertexAB)
    val productABAT: Vector = vectorAB.crossProduct(vectorAT)
    val signABAT = sign(productABAT.zComponent) //todo this way to tell direction of a vector might be wrong

    val vectorBT: Vector = new Vector(new Line2D(B, point))
    val vectorBC: Vector = new Vector(vertexBC)
    val productBCBT: Vector = vectorBC.crossProduct(vectorBT)
    val signBCBT = sign(productBCBT.zComponent)

    val vectorCT: Vector = new Vector(new Line2D(C, point))
    val vectorCA: Vector = new Vector(vertexCA)
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

  private def bothCovered(start: Point3D, end: Point3D): Boolean = {
    val startCovered = originalTriangle.isCloserThan(start)
    val endCovered = originalTriangle.isCloserThan(end)

    startCovered && endCovered
  }
  
  private def _cover(projectedLine: ProjectedLine): List[ProjectedLine] = {
    val original = List(projectedLine)

    var intersections = intersect(projectedLine)

    if (intersections.size == 1) {
      val i = intersections.toList(0)

      val theOnlyIntersectionIsAtTheEndOfTheLine = projectedLine.A == i || projectedLine.B == i
      if (theOnlyIntersectionIsAtTheEndOfTheLine) {
        val startOriginal = projectedLine.getOriginalPoint(projectedLine.A).get
        val endOriginal = projectedLine.getOriginalPoint(projectedLine.B).get

        if (oneEndIsOneThePlateAnotherIsCovered(projectedLine.A, projectedLine.B, startOriginal, endOriginal))
          return List()

        if (bothCovered(startOriginal, endOriginal))
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