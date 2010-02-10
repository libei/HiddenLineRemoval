package objectmother

import edu.ou.ece.dip.hlr.shape.{Point3D, Hexahedron}

object HexahedronMother {
  val length = 10
  val height = 10

  val pointA: Point3D = new Point3D(0, 0, 10)
  val pointB: Point3D = new Point3D(length, 0, 10)
  val pointC: Point3D = new Point3D(length, height, 10)
  val pointD: Point3D = new Point3D(0, height, 10)

  val offset = 0
  val pointE: Point3D = new Point3D(0 + offset, 0, 20)
  val pointF: Point3D = new Point3D(length + offset, 0, 20)
  val pointG: Point3D = new Point3D(length + offset, height, 20)
  val pointH: Point3D = new Point3D(0 + offset, height, 20)

  val regular = new Hexahedron(pointA, pointB, pointC, pointD, pointE, pointF, pointG, pointH)

  val pointACut = new Point3D(4, 0, 10)
  val cutRegular = new Hexahedron(pointACut, pointB, pointC, pointD, pointE, pointF, pointG, pointH)
}