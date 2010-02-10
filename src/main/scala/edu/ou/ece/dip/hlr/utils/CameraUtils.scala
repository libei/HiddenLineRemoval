package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.shape
import shape._
import collection.mutable.ListBuffer

object CameraUtils {
  def projectTriangle(triangle: Triangle3D, focalLength: Double): ProjectedTriangle = {

    new ProjectedTriangle(project(triangle.getVertexAB, focalLength),
      project(triangle.getVertexBC, focalLength),
      project(triangle.getVertexCA, focalLength), triangle
      )
  }

  def project(point: Point3D, focalLength: Double): Point2D = {
    new Point2D((focalLength * point.x) / (point.z),
      (focalLength * point.y) / (point.z))
  }

  def project(line: Line3D, focalLength: Double): ProjectedLine = {

    val start = project(line.A, focalLength)
    val end = project(line.B, focalLength)

    new ProjectedLine(start, end, line, focalLength)
  }

  def projectLines(lines: List[Line3D], focalLength: Double): List[ProjectedLine] = {
    val projectedLines = new ListBuffer[ProjectedLine]
    lines.foreach(l => {
      projectedLines.append(project(l, focalLength))
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