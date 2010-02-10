package objectmother

import edu.ou.ece.dip.hir.shape.{Point3D, Triangle3D}

object Triangle3DMother {
  val pointA: Point3D = new Point3D(0, 0, 10)
  val pointB: Point3D = new Point3D(5, 0, 10)
  val pointC: Point3D = new Point3D(2.5, 5, 10)

  val pointD: Point3D = new Point3D(10, 0, 10)
  val pointE: Point3D = new Point3D(7.5, 5, 10)

  val pointF: Point3D = new Point3D(15, 0, 10)
  val pointG: Point3D = new Point3D(12.5, 5, 10)

  val pointH: Point3D = new Point3D(20, 0, 10)
  val pointI: Point3D = new Point3D(17.5, 5, 10)

  val ABC = new Triangle3D(pointA, pointB, pointC)
  val BDE = new Triangle3D(pointB, pointD, pointE)
  val DFG = new Triangle3D(pointD, pointF, pointG)
  val FIG = new Triangle3D(pointF, pointI, pointG)


}