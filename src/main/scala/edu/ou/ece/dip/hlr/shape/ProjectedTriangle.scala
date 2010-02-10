package edu.ou.ece.dip.hlr.shape

import collection.mutable.{ListBuffer}
import edu.ou.ece.dip.hlr.utils.{FloatUtil, CameraProjectionUtils}

class ProjectedTriangle(vertexAB: ProjectedLine, vertexBC: ProjectedLine, vertexCA: ProjectedLine, val originalTriangle: Triangle3D) {
  val vertices = new ListBuffer[ProjectedLine]
  vertices.append(vertexAB)
  vertices.append(vertexBC)
  vertices.append(vertexCA)
  val cornerA = vertexAB.start
  val cornerB = vertexAB.end
  val cornerC = vertexBC.end

  def sign(x: Double): Double = {
    if (FloatUtil.Equals(x, 0))
      return 0
    Math.signum(x)
  }

  def isInside(point: Point2D): Boolean = {
    if ((point == cornerA) || (point == cornerB) || (point == cornerC))
      return true

    val vectorAT: Vector = new Vector(new Line2D(cornerA, point))
    val vectorAB: Vector = new Vector(vertexAB)
    val productABAT: Vector = vectorAB.crossProduct(vectorAT)
    val signABAT = sign(productABAT.zComponent) //todo this way to tell direction of a vector might be wrong

    val vectorBT: Vector = new Vector(new Line2D(cornerB, point))
    val vectorBC: Vector = new Vector(vertexBC)
    val productBCBT: Vector = vectorBC.crossProduct(vectorBT)
    val signBCBT = sign(productBCBT.zComponent)

    val vectorCT: Vector = new Vector(new Line2D(cornerC, point))
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
    isInside(line.start) && isInside(line.end)
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
    for (line <- coverInternal(projectedLine) if !FloatUtil.Equals(line.length, 0)) yield line
  }

  private def oneEndIsOneThePlateAnotherIsCovered(start: Point2D, end: Point2D, startOriginal: Point3D, endOriginal: Point3D): Boolean = {
    val isStartPointCovered = originalTriangle.isCloserThan(startOriginal) && isInside(start)
    val isStartPointOnThePlate = originalTriangle.isEqual(startOriginal) && isInside(start)
    val isStartPointUnCovered = originalTriangle.isBehind(startOriginal) || !isInside(start)

    val isEndPointCovered = originalTriangle.isCloserThan(endOriginal) && isInside(end)
    val isEndPointOnThePlate = originalTriangle.isEqual(endOriginal) && isInside(end)
    val isEndPointUnCovered = originalTriangle.isBehind(endOriginal) || !isInside(end)

    (isStartPointCovered && isEndPointOnThePlate) || (isStartPointOnThePlate && isEndPointCovered)
  }

  private def bothCovered(start: Point3D, end: Point3D): Boolean = {
    val startCovered = originalTriangle.isCloserThan(start)
    val endCovered = originalTriangle.isCloserThan(end)

    startCovered && endCovered
  }

  def coverInternal(projectedLine: ProjectedLine): List[ProjectedLine] = {
    val original = List(projectedLine)
    //
    //    if (isInside(projectedLine)
    //            && originalTriangle.isCloserThan(projectedLine.originalLine.start)
    //            && originalTriangle.isCloserThan(projectedLine.originalLine.end))
    //      return List()

    var intersections = intersect(projectedLine)

    if (intersections.size == 1) {
      val i = intersections.toList(0)

      val theOnlyIntersectionIsAtTheEndOfTheLine = projectedLine.start == i || projectedLine.end == i
      if (theOnlyIntersectionIsAtTheEndOfTheLine) {
        val startOriginal = projectedLine.getOriginalPoint(projectedLine.start).get
        val endOriginal = projectedLine.getOriginalPoint(projectedLine.end).get

        if (oneEndIsOneThePlateAnotherIsCovered(projectedLine.start, projectedLine.end, startOriginal, endOriginal))
          return List()

        if (bothCovered(startOriginal, endOriginal))
          return List()

        return original
      }

      val (endpointOutsideTriangle, originalEndpointInsideTriangle) =
      if (isInside(projectedLine.start))
        (projectedLine.end, projectedLine.getOriginalPoint(projectedLine.start))
      else
        (projectedLine.start, projectedLine.getOriginalPoint(projectedLine.end))

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
      //        if (!isInside(intersectionOne))
      //          true
      //        else
      //          originalTriangle.isBehind(originalPoint1.get) || originalTriangle.isEqual(originalPoint1.get)
      val isEndVisible = isPointVisible(intersectionTwo, originalPoint2.get)
      //
      //      val isEndVisible = originalTriangle.isBehind(originalPoint2.get) || originalTriangle.isEqual(originalPoint2.get)

      if (isStartVisible && isEndVisible)
        return original

      if (!isStartVisible || !isEndVisible)
        return List()

      if (intersectionOne.distanceTo(projectedLine.start) < intersectionTwo.distanceTo(projectedLine.start)) {
        return List(projectedLine.subLine(projectedLine.start, intersectionOne),
          projectedLine.subLine(intersectionTwo, projectedLine.end))
      } else {
        return List(projectedLine.subLine(projectedLine.start, intersectionTwo),
          projectedLine.subLine(intersectionOne, projectedLine.end))
      }
    }
    original
  }

  def isPointVisible(projectPoint: Point2D, originalPoint: Point3D): Boolean = {
    if (!isInside(projectPoint))
      true
    else
      {
        val isTiangleBehindThePoint: Boolean = originalTriangle.isBehind(originalPoint)
        val equal: Boolean = originalTriangle.isEqual(originalPoint)
        isTiangleBehindThePoint || equal
      }
  }
}