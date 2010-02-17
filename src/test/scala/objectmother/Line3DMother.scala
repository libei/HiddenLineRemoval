package objectmother

import edu.ou.ece.dip.hlr.shape.{Point3D, Line3D}

object Line3DMother {
    val lineAStart = new Point3D(0, 1, 12)
    val lineAEnd = new Point3D(30, 1, 12)
    val A = new Line3D(lineAStart, lineAEnd, "1")


    val lineBStart = new Point3D(0, 3, 12)
    val lineBEnd = new Point3D(30, 3, 12)
    val B = new Line3D(lineBStart, lineBEnd, "2")
}