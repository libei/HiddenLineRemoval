package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.shape.Line2D

object MatlabUtils {
  def generateMatrixForLineDisplay(lines: List[Line2D]): String  = {
    val builder: StringBuilder = new StringBuilder
    if(lines.length == 0)
      builder.append("---------empty----------")
    lines.foreach(l => {
      builder.append("line([" + l.A.x +" " + l.B.x + "], ["+ l.A.y +" " + l.B.y +"]);")
    })
    "figure;" + builder.toString
  }
}