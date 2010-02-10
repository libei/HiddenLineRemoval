package edu.ou.ece.dip.hir.shape

class ProjectedLine(start: Point2D, end: Point2D, originalLine: Line3D, val focalLength: Double) extends Line2D(start, end) {
  def subLine(start: Point2D, end: Point2D): ProjectedLine = {
    new ProjectedLine(start, end, originalLine, focalLength)
  }

  def getOriginalPoint(point: Point2D): Option[Point3D] = {
    val point1OnCamera = Point3D(point.x, point.y, focalLength)
    val lineFromZeroToPoint1OnCamera = Point3D(0, 0, 0) to point1OnCamera
    lineFromZeroToPoint1OnCamera intersectExtension originalLine
  }

  override def toString = "start: " + start.toString + " end:" + end.toString
}