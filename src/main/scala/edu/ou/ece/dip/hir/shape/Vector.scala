package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.utils.FloatUtil

class Vector(val xComponent: Double, val yComponent: Double, val zComponent: Double) {
  def this(line: Line3D) {
    this(line.end.x - line.start.x, line.end.y - line.start.y, line.end.z - line.start.z)
  }

  def this(line: Line2D) {
    this(line.end.x - line.start.x, line.end.y - line.start.y, 0)
  }

  def dotProduct(that: Vector): Double =
    this.xComponent * that.xComponent + this.yComponent * that.yComponent + this.zComponent * that.zComponent


  def crossProduct(that: Vector): Vector = {
        new Vector((this.yComponent * that.zComponent - this.zComponent * that.yComponent),
                    (this.zComponent * that.xComponent - this.xComponent * that.zComponent),
                    (this.xComponent * that.yComponent - this.yComponent * that.xComponent))
  }

  override def equals(obj: Any): Boolean = {
   if(obj == null)
      false
    if(!obj.isInstanceOf[Vector])
      false
    val that = obj.asInstanceOf[Vector]
    FloatUtil.Equals(this.xComponent, that.xComponent) && FloatUtil.Equals(this.yComponent, that.yComponent) && FloatUtil.Equals(this.zComponent, that.zComponent)
  }
}