package edu.ou.ece.dip.hir.shape

import scala.Math
import edu.ou.ece.dip.hir.utils.FloatUtil

object Line2D {
  def apply(start: Point2D, end: Point2D): Line2D = {
    new Line2D(start, end)
  }
}

class Line2D(val start: Point2D, val end: Point2D) {

  def slop: Option[Double] = {
    val deltaX = end.x- start.x
    val deltaY = end.y- start.y

    if(FloatUtil.Equals(deltaX, 0))
      return None

    Some(deltaY / deltaX)
  }

  def isParallelTo(that: Line2D): Boolean = {
    if(this.slop == None && that.slop == None)
      return true

    if(this.slop == None || that.slop == None)
      return false

    FloatUtil.Equals(this.slop.get, that.slop.get)    
  }


  def intersect(that: Line2D): Option[Point2D] = {

    if (FloatUtil.Equals(this.length, 0) || FloatUtil.Equals(that.length, 0))
      return None

    if(this.isParallelTo(that))
      return None

    val lambda: Double = -(-that.start.x * this.end.y + that.start.x * this.start.y - this.end.x * this.start.y + this.start.x * this.end.y + that.start.y * this.end.x - that.start.y * this.start.x) / (that.start.x * this.end.y - that.start.x * this.start.y - that.end.x * this.end.y + that.end.x * this.start.y - that.start.y * this.end.x + that.start.y * this.start.x + that.end.y * this.end.x - that.end.y * this.start.x)
    val miu: Double = (-that.start.x * this.start.y + that.end.y * that.start.x - that.end.x * that.start.y + that.end.x * this.start.y - that.end.y * this.start.x + that.start.y * this.start.x) / (that.start.x * this.end.y - that.start.x * this.start.y - that.end.x * this.end.y + that.end.x * this.start.y - that.start.y * this.end.x + that.start.y * this.start.x + that.end.y * this.end.x - that.end.y * this.start.x)

    val lambdaIsBetweenZeroAndOne: Boolean = (FloatUtil.LessThan(lambda, 1) || FloatUtil.Equals(lambda, 1)) && (FloatUtil.GreaterThan(lambda, 0) || FloatUtil.Equals(lambda, 0))
    val miuIsBetweenZeroAndOne: Boolean = (FloatUtil.LessThan(miu, 1) || FloatUtil.Equals(miu, 1)) && (FloatUtil.GreaterThan(miu, 0) || FloatUtil.Equals(miu, 0))
    if (lambdaIsBetweenZeroAndOne && miuIsBetweenZeroAndOne) {
      return Some(getPoint(miu))
    }
    return None
  }

  def getPoint(coefficient: Double): Point2D = {
    val x: Double = (1 - coefficient) * this.start.x + coefficient * this.end.x
    val y: Double = (1 - coefficient) * this.start.y + coefficient * this.end.y
    new Point2D(x, y)
  }

  def getCoefficient(point: Point2D): Double = {
    if (!FloatUtil.Equals((start.x - end.x), 0))
      return (start.x - point.x) / (start.x - end.x)

    if (!FloatUtil.Equals((start.y - end.y), 0))
      return (start.y - point.y) / (start.y - end.y)
    0
  }

  def intersectExtension(that: Line2D): Option[Point2D] = {
    val thisy1: Double = this.start.y
    val thisy2: Double = this.end.y
    val thisx1: Double = this.start.x
    val thisx2: Double = this.end.x

    val thaty1: Double = that.start.y
    val thaty2: Double = that.end.y
    val thatx1: Double = that.start.x
    val thatx2: Double = that.end.x

    if (FloatUtil.Equals(thisx1, thisx2 ) && FloatUtil.Equals(thatx1, thatx2)) {
      return None
    }

    val thisa = (thisy1 - thisy2) / (thisx1 - thisx2)
    val thisb = (thisx1 * thisy2 - thisy1 * thisx2) / (thisx1 - thisx2)
    val thata = (thaty1 - thaty2) / (thatx1 - thatx2)
    val thatb = (thatx1 * thaty2 - thaty1 * thatx2) / (thatx1 - thatx2)

    if (FloatUtil.Equals(thisa, thata)) {
      return None
    }

    if (FloatUtil.Equals(thisx1, thisx2)) {
      return Some(new Point2D(thisx1, thata * thisx1 + thatb))
    }

    if (FloatUtil.Equals(thatx1, thatx2)) {
      return Some(new Point2D(thatx1, thisa * thatx1 + thisb))
    }

    val x = (thatb - thisb) / (thisa - thata)
    val y = x * thata + thatb
    Some(new Point2D(x, y))
  }

  def length = Math.sqrt((end.x - start.x) * (end.x - start.x) + (end.y - start.y) * (end.y - start.y)).asInstanceOf[Double]

  override def equals(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Line2D])
      false
    val that = obj.asInstanceOf[Line2D]
    ((this.start == that.start) && (this.end == that.end))
  }

  override def hashCode: Int = {
    start.hashCode + end.hashCode
  }
}