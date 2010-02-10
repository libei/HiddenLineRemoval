package objectmother

import edu.ou.ece.dip.hlr.shape.{Line3D, ProjectedTriangle, ProjectedLine, Point2D}

object ProjectedTriangleMother {
  def create(A: Point2D, B: Point2D, C: Point2D): ProjectedTriangle = {
    new ProjectedTriangle(ProjectedLineMother.create(A, B),
      ProjectedLineMother.create(B, C),
      ProjectedLineMother.create(C, A),
      null)
  }
}

object ProjectedLineMother {
  def create(start: Point2D, end: Point2D): ProjectedLine = {
    create(start, end, null)
  }

  def create(start: Point2D, end: Point2D, original: Line3D): ProjectedLine = {
    new ProjectedLine(start, end, original, 0.0)
  }

}