package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.utils.FloatUtil

object Point3D {
  def apply(x: Double, y: Double, z: Double): Point3D = {
    new Point3D(x, y, z)
  }

  def apply(x: Double, y: Double, z: Double, name: String): Point3D = {
    new Point3D(x, y, z, name)
  }
}

class Point3D(val x: Double, val y: Double, val z: Double, val name: String) {

  def this(x: Double, y: Double, z: Double) {
    this(x, y, z, "")
  }

  

  override def equals(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Point3D])
      false
    val that = obj.asInstanceOf[Point3D]
    FloatUtil.Equals(this.x, that.x) && FloatUtil.Equals(this.y, that.y) && FloatUtil.Equals(this.z, that.z)
  }

  override def hashCode: Int = {
    var result: Int = 0
    var temp: Long = 0L
    temp = if (x != +0.0d) java.lang.Double.doubleToLongBits(x) else 0L
    result = (temp ^ (temp >>> 32)).asInstanceOf[Int]
    temp = if (y != +0.0d) java.lang.Double.doubleToLongBits(y) else 0L
    result = 31 * result + (temp ^ (temp >>> 32)).asInstanceOf[Int]
    temp = if (z != +0.0d) java.lang.Double.doubleToLongBits(z) else 0L
    result = 31 * result + (temp ^ (temp >>> 32)).asInstanceOf[Int]
    return result
  }

  override def toString = "Name:" + name +" x: " + x + " y: " + y + " z: " + z
}