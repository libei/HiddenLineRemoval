package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.shape
import shape._
import collection.mutable.ListBuffer

object CameraProjectionUtils {
  def projectTriangle(triangle: Triangle3D, focalLength: Double): ProjectedTriangle = {

    new ProjectedTriangle(projectLine(triangle.getVertexAB, focalLength),
      projectLine(triangle.getVertexBC, focalLength),
      projectLine(triangle.getVertexCA, focalLength), triangle
      )
  }

  def projectPoint(point: Point3D, focalLength: Double): Point2D = {
    new Point2D((focalLength * point.x) / (point.z),
      (focalLength * point.y) / (point.z))
  }

  def projectLine(line: Line3D, focalLength: Double): ProjectedLine = {

    val start = projectPoint(line.start, focalLength)
    val end = projectPoint(line.end, focalLength)

    new ProjectedLine(start, end, line, focalLength)
  }

  def projectLines(lines: List[Line3D], focalLength: Double): List[ProjectedLine] = {
    val projectedLines = new ListBuffer[ProjectedLine]
    lines.foreach(l => {
      projectedLines.append(projectLine(l, focalLength))
    })
    projectedLines.toList
  }

  def projectTriangles(triangles: List[Triangle3D], focalLength: Double): List[ProjectedTriangle] = {
    val projectedTriangles = new ListBuffer[ProjectedTriangle]
    triangles.foreach(t => {
      projectedTriangles.append(projectTriangle(t, focalLength))
    })
    projectedTriangles.toList
  }

}