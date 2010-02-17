package edu.ou.ece.dip.hlr.utils

import collection.mutable.ListBuffer
import edu.ou.ece.dip.hlr.shape._

case class PointDto(x: Double, y: Double, z: Double, index: Int, shapeId: Int)
case class LineDto(s_x: Double, s_y: Double, s_z: Double, e_x: Double, e_y: Double, e_z: Double, index: Int)

object MatlabInterop {
  def generateMatrixForLineDisplay(lines: List[Line2D]): String = {
    val builder: StringBuilder = new StringBuilder
    if (lines.length == 0)
      builder.append("---------empty----------")
    lines.foreach(l => {
      builder.append("line([" + l.A.x + " " + l.B.x + "], [" + l.A.y + " " + l.B.y + "]);")
    })
    "figure;" + builder.toString
  }

  def parseShapes(x: Array[Double], y: Array[Double], z: Array[Double],
                  index: Array[Int], id: Array[Int]): List[Decomposable] = {
    val points = new ListBuffer[PointDto];
    for (i <- 0 until x.length) {
      points += new PointDto(x(i), y(i), z(i), index(i), id(i))
    }

    val shapes = new ListBuffer[Decomposable]
    var pointsToProcess = points.toList

    while (pointsToProcess.length != 0) {
      val res = extraHead(pointsToProcess)
      shapes += res._1
      pointsToProcess = res._2
    }

    shapes.toList
  }

  def parseLines(line_start_x: Array[Double], line_start_y: Array[Double], line_start_z: Array[Double],
                 line_end_x: Array[Double], line_end_y: Array[Double], line_end_z: Array[Double],
                 line_index: Array[Int]): List[LineDto] = {

    //can't use for...yield since a scala bug http://www.eishay.com/2009/06/unexpected-repeated-execution-in-scala.html
    val res = new ListBuffer[LineDto]

    val value = for (i <- 0 until line_start_x.length) {
      res.append(LineDto(line_start_x(i), line_start_y(i), line_start_z(i), line_end_x(i), line_end_y(i), line_end_z(i), line_index(i)))
    }

    res.toList
  }

  def removeHiddenLine(shape_x: Array[Double], shape_y: Array[Double], shape_z: Array[Double],
                       index: Array[Int], shapeId: Array[Int],
                       line_start_x: Array[Double], line_start_y: Array[Double], line_start_z: Array[Double],
                       line_end_x: Array[Double], line_end_y: Array[Double], line_end_z: Array[Double],
                       line_index: Array[Int],
                       out_start_x: Array[Double], out_start_y: Array[Double],
                       out_end_x: Array[Double], out_end_y: Array[Double],
                       original_index: Array[Int]) {
    //    val decomopsables = parseShapes(shape_x, shape_y, shape_z, index, shapeId)
    //    val triangles = for(d <- decomopsables) yield d.triangles
    //    val lines = parseLines(shape_x, shape_y, shape_z, index, shapeId)
    //    HiddenLineRemovalUtils.removeHiddenLines(triangles, lines, focalLength)
  }

  private def toPoint(p: PointDto): Point3D = {
    Point3D(p.x, p.y, p.z, p.index.toString)
  }

  private def extraHead(points: List[PointDto]): Tuple2[Decomposable, List[PointDto]] = {
    val shapeId = points(0).shapeId
    var decomposable: Decomposable = null
    shapeId match {
      case 3 => {
        val A = points(0)
        val B = points(1)
        val C = points(2)
        decomposable = new Triangle3D(toPoint(A), toPoint(B), toPoint(C))
      }

      case 4 => {
        val A = points(0)
        val B = points(1)
        val C = points(2)
        val D = points(3)
        decomposable = new Trapezoid3D(toPoint(A), toPoint(B), toPoint(C), toPoint(D))
      }
      case _ => {
        throw new Exception("Invalid shape ID")
      }
    }
    return (decomposable, points.slice(points(0).shapeId).toList)
  }
}