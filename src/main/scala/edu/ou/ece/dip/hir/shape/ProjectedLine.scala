package edu.ou.ece.dip.hir.shape

class ProjectedLine(start: Point2D, end: Point2D, originalLine: Line3D, val focalLength: Double) extends Line2D(start, end) {
  def subLine(start: Point2D, end: Point2D): ProjectedLine = {

    val newOriginalStart = getOriginalPoint(start)
    val newOriginalEnd = getOriginalPoint(end)
    //todo
    new ProjectedLine(start, end, originalLine, focalLength)
  }

  def getOriginalPoint(point: Point2D): Option[Point3D] = {
    val point1OnCamera = new Point3D(point.x, point.y, focalLength)
    val lineFromZeroToPoint1OnCamera = new Line3D(new Point3D(0, 0, 0), point1OnCamera)
    lineFromZeroToPoint1OnCamera.intersectExtension(originalLine)
  }

  override def toString = "start: " + start.toString + " end:" + end.toString
}