package edu.ou.ece.dip.hlr.shape

import org.specs.runner.ScalaTest
import edu.ou.ece.dip.hlr.SpecificationBase
import edu.ou.ece.dip.hlr.utils.{CameraUtils}

class ProjectedLineSpec extends SpecificationBase {

  "One line parallel to X axis and one line parallel Y axis" in {
    val lineA = new ProjectedLine(new Point2D(1, 0), new Point2D(1, 2), null, 0.5)
    val lineB = new ProjectedLine(new Point2D(0, 1), new Point2D(2, 1), null, 0.5)

    val actual = lineA.intersect(lineB)

    actual.get.x must_== 1
    actual.get.y must_== 1
  }

  "One line parallel to X axis and one line doesn't not parallel to any axis" in {
    val lineA = new ProjectedLine(new Point2D(0, 0), new Point2D(2.5, 5), null, 0.5)
    val linePrallelToX = new ProjectedLine(new Point2D(0, 3), new Point2D(5, 3), null, 0.5)
    val intersection = lineA.intersect(linePrallelToX)
    intersection.get.x must_== 1.5
    intersection.get.y must_== 3

  }

  "Two lines that do not intersect should have no intersection" in {
    val lineA = new ProjectedLine(new Point2D(1, 0), new Point2D(1, 2), null, 0.5)
    val lineB = new ProjectedLine(new Point2D(2, 0), new Point2D(2, 2), null, 0.5)

    val actual = lineA.intersect(lineB)

    actual must_== None
  }

  "Could get subline" in {

    val originalLine: Line3D = new Line3D(new Point3D(1, 0, 5), new Point3D(1, 2, 5))
    val projected = new ProjectedLine(new Point2D(1, 0), new Point2D(1, 2), originalLine, 0.5)
    val newStart = new Point2D(1, 0.5)
    val newEnd = new Point2D(1, 1.5)
    val subLine = projected.subLine(newStart, newEnd)
    subLine.A must_== newStart
    subLine.B must_== newEnd
    subLine.getOriginalPoint(subLine.A) must_== new Point3D(1, 0.5, 5)
    subLine.getOriginalPoint(subLine.B) must_== new Point3D(1, 1.5, 5)
  }

  "Should map a point to original point" in {
    val originalLine = new Line3D(new Point3D(-1, 0, 5), new Point3D(1, 0, 5))
    val projectedLine = CameraUtils.project(originalLine, 0.5)
    projectedLine.getOriginalPoint(projectedLine.A).get must_== originalLine.A
    projectedLine.getOriginalPoint(projectedLine.B).get must_== originalLine.B
  }
  
}