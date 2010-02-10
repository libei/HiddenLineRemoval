package edu.ou.ece.dip.hlr.utils

object FloatUtil {

  val EPSION = 5E-8
  
  def Equals(thiz: Double, that: Double): Boolean = Math.abs(thiz - that) <= EPSION

  def GreaterThan(thiz: Double, that: Double): Boolean = (Math.abs(thiz - that) > EPSION) && (thiz - that > 0)

  def LessThan(thiz: Double, that: Double): Boolean = (Math.abs(thiz - that) > EPSION) && (thiz - that < 0)
}