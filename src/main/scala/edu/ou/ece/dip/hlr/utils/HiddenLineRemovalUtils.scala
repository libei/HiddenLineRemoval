package edu.ou.ece.dip.hlr.utils

import collection.mutable.ListBuffer
import edu.ou.ece.dip.hlr.shape._

object HiddenLineRemovalUtils {
  def removeHiddenLines(triangles: List[Triangle3D], lines: List[Line3D], focalLength: Double): List[ProjectedLine] = {

    var currentLines = CameraUtils.projectLines(lines, focalLength)
    Console.out.println(MatlabUtils.generateMatrixForLineDisplay(currentLines))

    val projectedTriangles = CameraUtils.projectTriangles(triangles, focalLength)
    projectedTriangles.foreach(t => {
      currentLines = removeHiddenLinesForOne(t, currentLines)
      Console.out.println(t.originalTriangle.toString)
      Console.out.println(MatlabUtils.generateMatrixForLineDisplay(currentLines))
    })

    currentLines.toList
  }

  private def removeHiddenLinesForOne(triangle: ProjectedTriangle, lines: List[ProjectedLine]): List[ProjectedLine] = {

    val newLines = new ListBuffer[ProjectedLine]();
    lines.foreach(l => {
      newLines.appendAll(triangle.cover(l))
    })
    LineUtils.removeDuplicated(LineUtils.removeZeroLength(newLines.toList))
  }
}