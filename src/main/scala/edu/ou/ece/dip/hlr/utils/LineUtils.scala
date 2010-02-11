package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.shape.ProjectedLine
import collection.mutable.ListBuffer

object LineUtils {
  def removeDuplicated(lines: List[ProjectedLine]): List[ProjectedLine] = {
    val res = new ListBuffer[ProjectedLine];

    lines.foreach(line => {
      if(!res.exists(l => {
        (line equalsIgnoreDirection l)
      }))
        res += line
    })

    res.toList
  }

  def removeZeroLength(lines: List[ProjectedLine]): List[ProjectedLine] = {
    lines.filter(l => {
      !FloatUtil.Equals(l.length, 0)
    })
  }

}