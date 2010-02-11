package edu.ou.ece.dip.hlr.shape

import scala.Math
import edu.ou.ece.dip.hlr.utils.FloatUtil

class Line2D(val A: Point2D, val B: Point2D) {

  def slop: Option[Double] = {
    val deltaX = B.x- A.x
    val deltaY = B.y- A.y

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

    val lambda: Double = -(-that.A.x * this.B.y + that.A.x * this.A.y - this.B.x * this.A.y + this.A.x * this.B.y + that.A.y * this.B.x - that.A.y * this.A.x) / (that.A.x * this.B.y - that.A.x * this.A.y - that.B.x * this.B.y + that.B.x * this.A.y - that.A.y * this.B.x + that.A.y * this.A.x + that.B.y * this.B.x - that.B.y * this.A.x)
    val miu: Double = (-that.A.x * this.A.y + that.B.y * that.A.x - that.B.x * that.A.y + that.B.x * this.A.y - that.B.y * this.A.x + that.A.y * this.A.x) / (that.A.x * this.B.y - that.A.x * this.A.y - that.B.x * this.B.y + that.B.x * this.A.y - that.A.y * this.B.x + that.A.y * this.A.x + that.B.y * this.B.x - that.B.y * this.A.x)

    val lambdaIsBetweenZeroAndOne: Boolean = (FloatUtil.LessThan(lambda, 1) || FloatUtil.Equals(lambda, 1)) && (FloatUtil.GreaterThan(lambda, 0) || FloatUtil.Equals(lambda, 0))
    val miuIsBetweenZeroAndOne: Boolean = (FloatUtil.LessThan(miu, 1) || FloatUtil.Equals(miu, 1)) && (FloatUtil.GreaterThan(miu, 0) || FloatUtil.Equals(miu, 0))
    if (lambdaIsBetweenZeroAndOne && miuIsBetweenZeroAndOne) {
      return Some(getPoint(miu))
    }
    return None
  }

  def getPoint(coefficient: Double): Point2D = {
    val x: Double = (1 - coefficient) * this.A.x + coefficient * this.B.x
    val y: Double = (1 - coefficient) * this.A.y + coefficient * this.B.y
    new Point2D(x, y)
  }

  def getCoefficient(point: Point2D): Double = {
    if (!FloatUtil.Equals((A.x - B.x), 0))
      return (A.x - point.x) / (A.x - B.x)

    if (!FloatUtil.Equals((A.y - B.y), 0))
      return (A.y - point.y) / (A.y - B.y)
    0
  }

  def intersectExtension(that: Line2D): Option[Point2D] = {
    val thisy1: Double = this.A.y
    val thisy2: Double = this.B.y
    val thisx1: Double = this.A.x
    val thisx2: Double = this.B.x

    val thaty1: Double = that.A.y
    val thaty2: Double = that.B.y
    val thatx1: Double = that.A.x
    val thatx2: Double = that.B.x

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

  def length = Math.sqrt((B.x - A.x) * (B.x - A.x) + (B.y - A.y) * (B.y - A.y)).asInstanceOf[Double]

  def equalsIgnoreDirection(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Line2D])
      false
    val that = obj.asInstanceOf[Line2D]
    ((this.A == that.A) && (this.B == that.B)) || ((this.A == that.B) && (this.B == that.A)) 

  }

  override def equals(obj: Any): Boolean = {
    if (obj == null)
      false
    if (!obj.isInstanceOf[Line2D])
      false
    val that = obj.asInstanceOf[Line2D]
    ((this.A == that.A) && (this.B == that.B))
  }

  override def hashCode: Int = {
    A.hashCode + B.hashCode
  }
}