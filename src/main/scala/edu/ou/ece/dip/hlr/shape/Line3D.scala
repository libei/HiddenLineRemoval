package edu.ou.ece.dip.hlr.shape

class Line3D(val A: Point3D, val B: Point3D) {
  
  private def tryIntersect(thisStartX: Double, thisStartY: Double, thisEndX: Double, thisEndY: Double, thatStartX: Double, thatStartY: Double, thatEndX: Double, thatEndY: Double): Option[Point3D] = {

    val lineThisStartXY = Point2D(thisStartX, thisStartY)
    val lineThisEndXY = Point2D(thisEndX, thisEndY)
    val lineThisXY = lineThisStartXY to lineThisEndXY

    val lineThatStartXY = Point2D(thatStartX, thatStartY)
    val lineThatEndXY = Point2D(thatEndX, thatEndY)
    val lineThatXY = lineThatStartXY to lineThatEndXY

    val intersection = lineThisXY.intersectExtension(lineThatXY)
    if (intersection != None) {
      val coefficient: Double = lineThisXY.getCoefficient(intersection.get)
      return Some(getPoint(coefficient))
    }
    None
  }

  def intersectExtension(that: Line3D): Option[Point3D] = {
//    val intersectXY = tryIntersect(this.A.x, this.A.y, this.B.x, this.B.y, that.A.x, that.A.y, that.B.x, that.B.y)

    val intersectXZ = tryIntersect(this.A.x, this.A.z, this.B.x, this.B.z, that.A.x, that.A.z, that.B.x, that.B.z)

    val intersectZY = tryIntersect(this.A.z, this.A.y, this.B.z, this.B.y, that.A.z, that.A.y, that.B.z, that.B.y)
    
//    if (intersectXY != None)
//      return intersectXY

    if (intersectXZ != None)
      return intersectXZ

    if (intersectZY != None)
      return intersectZY

    None
  }


  def getPoint(coefficient: Double): Point3D = {
    val x: Double = (1 - coefficient) * this.A.x + coefficient * this.B.x
    val y: Double = (1 - coefficient) * this.A.y + coefficient * this.B.y
    val z: Double = (1 - coefficient) * this.A.z + coefficient * this.B.z
    new Point3D(x, y, z)
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Line3D])
      false
    val that = obj.asInstanceOf[Line3D]
    ((this.A == that.A) && (this.B == that.B))
  }

  override def hashCode: Int = {
    A.hashCode + B.hashCode
  }
}