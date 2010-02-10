package edu.ou.ece.dip.hir.shape

object Line3D {
  def apply(start: Point3D, end: Point3D): Line3D = {
    new Line3D(start, end)
  }
}

class Line3D(val start: Point3D, val end: Point3D) {
  
  private def tryIntersect(thisStartX: Double, thisStartY: Double, thisEndX: Double, thisEndY: Double, thatStartX: Double, thatStartY: Double, thatEndX: Double, thatEndY: Double): Option[Point3D] = {

    val lineThisStartXY = new Point2D(thisStartX, thisStartY)
    val lineThisEndXY = new Point2D(thisEndX, thisEndY)
    val lineThisXY = new Line2D(lineThisStartXY, lineThisEndXY)

    val lineThatStartXY = new Point2D(thatStartX, thatStartY)
    val lineThatEndXY = new Point2D(thatEndX, thatEndY)
    val lineThatXY = new Line2D(lineThatStartXY, lineThatEndXY)

    val intersection = lineThisXY.intersectExtension(lineThatXY)
    if (intersection != None) {
      val coefficient: Double = lineThisXY.getCoefficient(intersection.get)
      return Some(getPoint(coefficient))
    }
    None
  }

  def intersectExtension(that: Line3D): Option[Point3D] = {
//    val intersectXY = tryIntersect(this.start.x, this.start.y, this.end.x, this.end.y, that.start.x, that.start.y, that.end.x, that.end.y)

    val intersectXZ = tryIntersect(this.start.x, this.start.z, this.end.x, this.end.z, that.start.x, that.start.z, that.end.x, that.end.z)

    val intersectZY = tryIntersect(this.start.z, this.start.y, this.end.z, this.end.y, that.start.z, that.start.y, that.end.z, that.end.y)
    
//    if (intersectXY != None)
//      return intersectXY

    if (intersectXZ != None)
      return intersectXZ

    if (intersectZY != None)
      return intersectZY

    None
  }


  def getPoint(coefficient: Double): Point3D = {
    val x: Double = (1 - coefficient) * this.start.x + coefficient * this.end.x
    val y: Double = (1 - coefficient) * this.start.y + coefficient * this.end.y
    val z: Double = (1 - coefficient) * this.start.z + coefficient * this.end.z
    new Point3D(x, y, z)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Line3D])
      false
    val that = obj.asInstanceOf[Line3D]
    ((this.start == that.start) && (this.end == that.end))
  }

  override def hashCode: Int = {
    start.hashCode + end.hashCode
  }
}