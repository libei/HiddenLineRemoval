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

  def parseLines(start_x: Array[Double], start_y: Array[Double], start_z: Array[Double],
                 end_x: Array[Double], end_y: Array[Double], end_z: Array[Double],
                 index: Array[Int]): List[Line3D] = {

    //can't use for...yield since a scala bug http://www.eishay.com/2009/06/unexpected-repeated-execution-in-scala.html
    val res = new ListBuffer[Line3D]

    val value = for (i <- 0 until start_x.length) {
      val A = Point3D(start_x(i), start_y(i), start_z(i))
      val B = Point3D(end_x(i), end_y(i), end_z(i))
      val line = new Line3D(A, B, index(i).toString)
      res.append(line)
    }

    res.toList
  }

  def generateRes(lines: List[ProjectedLine], out_start_x: Array[Double], out_start_y: Array[Double],
                  out_end_x: Array[Double], out_end_y: Array[Double],
                  original_index: Array[Int]) {

    for(i <- 0 until lines.length) {
      out_start_x(i) = lines(i).A.x
      out_start_y(i) = lines(i).A.y
      out_end_x(i) = lines(i).B.x
      out_end_y(i) = lines(i).B.y
      original_index(i) = Integer.parseInt(lines(i).name)

    }
    
  }

  def removeHiddenLine(shape_x: Array[Double], shape_y: Array[Double], shape_z: Array[Double],
                       index: Array[Int], shapeId: Array[Int],
                       line_start_x: Array[Double], line_start_y: Array[Double], line_start_z: Array[Double],
                       line_end_x: Array[Double], line_end_y: Array[Double], line_end_z: Array[Double],
                       line_index: Array[Int],
                       out_start_x: Array[Double], out_start_y: Array[Double],
                       out_end_x: Array[Double], out_end_y: Array[Double],
                       original_index: Array[Int], focalLength: Double) {

    val decomopsables = parseShapes(shape_x, shape_y, shape_z, index, shapeId)
    val triangles = for (d <- decomopsables) yield d.triangles
    val lines = parseLines(line_start_x, line_start_y, line_start_z, line_end_x, line_end_y, line_end_z, line_index)
    val newLines = HiddenLineRemovalUtils.removeHiddenLines(triangles.flatten, lines, focalLength)

    generateRes(newLines, out_start_x, out_start_y, out_end_x, out_end_y, original_index)
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