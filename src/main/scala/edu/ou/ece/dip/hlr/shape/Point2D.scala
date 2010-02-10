package edu.ou.ece.dip.hlr.shape

import edu.ou.ece.dip.hlr.utils.FloatUtil

object Point2D {
  def apply(x: Double, y: Double): Point2D = {
    new Point2D(x, y)
  }
}

class Point2D (val x: Double, val y: Double) {

  def to(that: Point2D): Line2D = {
    new Line2D(this, that)
  }

  def distanceTo(that: Point2D): Double = {
    Math.sqrt((this.x - that.x)*(this.x - that.x) + (this.y - that.y)*(this.y - that.y)).toFloat 
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Point2D])
      false
    val that = obj.asInstanceOf[Point2D]
    FloatUtil.Equals(this.x, that.x) && FloatUtil.Equals(this.y, that.y)
  }

  override def hashCode: Int = {
    var result: Int = 0
    var temp: Long = 0L
    temp = if (x != +0.0d) java.lang.Double.doubleToLongBits(x) else 0L
    result = (temp ^ (temp >>> 32)).asInstanceOf[Int]
    temp = if (y != +0.0d) java.lang.Double.doubleToLongBits(y) else 0L
    result = 31 * result + (temp ^ (temp >>> 32)).asInstanceOf[Int]
    return result
  }
  
  override def toString = "x: " + x + " y: " + y
}