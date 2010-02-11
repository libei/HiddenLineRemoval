package edu.ou.ece.dip.hlr.utils

import edu.ou.ece.dip.hlr.SpecificationBase
import edu.ou.ece.dip.hlr.shape._
import objectmother.{Line3DMother, Triangle3DMother, HexahedronMother}
import collection.mutable.ListBuffer

class HiddenLineRemovalUtilsSpec extends SpecificationBase {
  "Should remove hidden lines for regular Hexahedron" in {
    val regular: Hexahedron = HexahedronMother.regular

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(regular.composingTriangles, regular.composingVertices, 0.5)

    afterRemovalLines.length must_== 7
  }

  "Given four triangles and a line goes through them, the covered part of the line should be invisible" in {

    val triangles = List(Triangle3DMother.ABC, Triangle3DMother.BDE, Triangle3DMother.DFG, Triangle3DMother.FIG)
    val lines = Triangle3DMother.ABC.vertices ::: Triangle3DMother.BDE.vertices ::: Triangle3DMother.DFG.vertices ::: Triangle3DMother.FIG.vertices ::: List(Line3DMother.A)

    val actual = HiddenLineRemovalUtils.removeHiddenLines(triangles, lines, 0.5)

    actual.length must_== 16

  }

  "Given a triangle and line starting from one corner of that triangle going behind, the line should be invisible" in {

    val pointAway = new Point3D(2.5, 2.5, 15)
    val triangles = List(Triangle3DMother.ABC)
    val lineAAway = new Line3D(Triangle3DMother.ABC.cornerA, pointAway)
    val lines = Triangle3DMother.ABC.vertices ::: List(lineAAway)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(triangles, lines, 0.5)

    afterRemovalLines.length must_== 3
  }

  "Given one triangle and two lines go through it" in {

    val triangles = List(Triangle3DMother.ABC)
    val lines = Triangle3DMother.ABC.vertices ::: List(Line3DMother.A) ::: List(Line3DMother.B)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(triangles, lines, 0.5)

    afterRemovalLines.length must_== 7
  }

  "Given a centrum" in {
    val A = new Point3D(7.5, 5, 3.5)
    val B = new Point3D(5, 0, 1)
    val C = new Point3D(10, 0, 1)
    val D = new Point3D(2.5, 2, 6)

    val ABC = new Triangle3D(A, B, C)
    val ADC = new Triangle3D(A, D, C)
    val ABD = new Triangle3D(A, B, D)
    val DBC = new Triangle3D(D, B, C)

    val lineAB = new Line3D(A, B)
    val lineAC = new Line3D(A, C)
    val lineBC = new Line3D(B, C)
    val lineAD = new Line3D(A, D)
    val lineBD = new Line3D(B, D)
    val lineCD = new Line3D(C, D)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(List(ABC, ABD, ADC, DBC), List(lineAB, lineAC, lineBC, lineAD, lineBD, lineCD), 0.5)

    afterRemovalLines.length must_== 5

  }

  "Given another centrum" in {
    val A = new Point3D(7.5, 5, 3.5)
    val B = new Point3D(5, 0, 1)
    val C = new Point3D(10, 0, 1)
    val D = new Point3D(2.5, 2, 6)

    val ABC = new Triangle3D(A, B, C)
    val ADC = new Triangle3D(A, D, C)
    val ABD = new Triangle3D(A, B, D)
    val DBC = new Triangle3D(D, B, C)

    val lineAB = new Line3D(A, B)
    val lineAC = new Line3D(A, C)
    val lineBC = new Line3D(B, C)
    val lineAD = new Line3D(A, D)
    val lineBD = new Line3D(B, D)
    val lineCD = new Line3D(C, D)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(List(ABC, ABD, ADC, DBC), List(lineAB, lineAC, lineBC, lineAD, lineBD, lineCD), 0.5)

    afterRemovalLines.length must_== 5

  }

  "Given a strange shape" in {
    val A = new Point3D(4, 4, 10)
    val B = new Point3D(10, 4, 10)
    val C = new Point3D(10, 6, 10)
    val D = new Point3D(4, 6, 10)

    val E = new Point3D(2, 2, 20)
    val F = new Point3D(6, 2, 20)
    val G = new Point3D(6, 8, 20)
    val H = new Point3D(2, 8, 20)

    val hex = new Hexahedron(A, B, C, D, E, F, G, H)

    val t = new Trapezoid3D(A, B, C, D)
    //    var afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(List(Triangle3D(A, C, D)), List(Line3D(G, C)), 0.5)
    //    var afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, List(Line3D(C, G), Line3D(G, C), Line3D(H, G), Line3D(G, F)), 0.5)
    //    afterRemovalLines.length must_== 0
    //    afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, List(Line3D(A, B), Line3D(B, C), Line3D(C, D), Line3D(D, A), Line3D(D, H), Line3D(A, E), Line3D(B, F)), 0.5)
    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, hex.composingVertices, 0.5)
    afterRemovalLines.length must_== 14
  }

  "Given a strange shape 1" in {
    val A = Point3D(2, 2, 10, "A")
    val B = Point3D(6, 2, 10, "B")
    val C = Point3D(6, 6, 10, "C")
    val D = Point3D(2, 6, 10, "D")

    val E = Point3D(12, 2 + -10, 20, "E")
    val F = Point3D(18, 2 + -10, 20, "F")
    val G = Point3D(18, 6 + -10, 20, "G")
    val H = Point3D(12, 6 + -10, 20, "H")

    val AB = A to B
    val ABCD = new Trapezoid3D(A, B, C, D)

    val hex = new Hexahedron(A, B, C, D, E, F, G, H)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, hex.composingVertices, 0.5)
    afterRemovalLines.length must_== 14
  }

  "Given a strange shape 2" in {
    val A = Point3D(2, 2, 10, "A")
    val B = Point3D(6, 2, 10, "B")
    val C = Point3D(6, 6, 10, "C")
    val D = Point3D(2, 6, 10, "D")

    val E = Point3D(12 - 20, 2, 20, "E")
    val F = Point3D(18 - 20, 2, 20, "F")
    val G = Point3D(18 - 20, 6, 20, "G")
    val H = Point3D(12 - 20, 6, 20, "H")

    val AB = A to B
    val ABCD = new Trapezoid3D(A, B, C, D)

    val hex = new Hexahedron(A, B, C, D, E, F, G, H)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, hex.composingVertices, 0.5)
    afterRemovalLines.length must_== 14
  }

  "Given a strange shape 3" in {
    val A = Point3D(2, 2, 10, "A")
    val B = Point3D(6, 2, 10, "B")
    val C = Point3D(6, 6, 10, "C")
    val D = Point3D(2, 6, 10, "D")

    val E = Point3D(12 - 20, 2 + 20, 20, "E")
    val F = Point3D(18 - 20, 2 + 20, 20, "F")
    val G = Point3D(18 - 20, 6 + 20, 20, "G")
    val H = Point3D(12 - 20, 6 + 20, 20, "H")

    val AB = A to B
    val ABCD = new Trapezoid3D(A, B, C, D)

    val hex = new Hexahedron(A, B, C, D, E, F, G, H)

    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, hex.composingVertices, 0.5)
    afterRemovalLines.length must_== 14
  }

  "Given a strange shape 3 with fraction" in {
    val A = Point3D(2.634300, 2.634300, 10.634300, "A")
    val B = Point3D(6.634300, 2.634300, 10.634300, "B")
    val C = Point3D(6.634300, 6.634300, 10.634300, "C")
    val D = Point3D(2.634300, 6.634300, 10.634300, "D")

    val E = Point3D(12.634300f - 20.0, 2.634300f + 20.0, 20.634300, "E")
    val F = Point3D(18.634300f - 20.0, 2.634300f + 20.0, 20.634300, "F")
    val G = Point3D(18.634300f - 20.0, 6.634300f + 20.0, 20.634300, "G")
    val H = Point3D(12.634300f - 20.0, 6.634300f + 20.0, 20.634300, "H")

    val AB = A to B
    val ABCD = new Trapezoid3D(A, B, C, D)

    val hex = new Hexahedron(A, B, C, D, E, F, G, H)

    //    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, List(Line3D(G, H)), 0.5)
    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(hex.composingTriangles, hex.composingVertices, 0.5)
    afterRemovalLines.length must_== 14
  }

  "XXXXXXXX" in {
    skip("those examples don't pass yet")
    val v_1 = new Point3D(18.672503f, 1.031470f, 113.021418f, "v_1")
    val v_2 = new Point3D(20.673238f, 0.000000f, 107.524443f, "v_2")
    val v_3 = new Point3D(18.868493f, 4.281336f, 112.482937f, "v_3")
    val v_4 = new Point3D(20.869229f, 3.249866f, 106.985963f, "v_4")
    val v_5 = new Point3D(13.662148f, 5.364624f, 110.384711f, "v_5")
    val v_6 = new Point3D(15.662883f, 4.333154f, 104.887736f, "v_6")
    val v_7 = new Point3D(11.092381f, 9.165136f, 108.118151f, "v_7")
    val v_8 = new Point3D(12.707789f, 8.332320f, 103.679854f, "v_8")
    val v_9 = new Point3D(3.960114f, 9.165136f, 105.522219f, "v_9")
    val v_10 = new Point3D(5.575522f, 8.332320f, 101.083921f, "v_10")
    val v_11 = new Point3D(1.258206f, 5.364624f, 105.870045f, "v_11")
    val v_12 = new Point3D(3.258941f, 4.333154f, 100.373070f, "v_12")
    val v_13 = new Point3D(-1.765546f, 4.931309f, 104.850798f, "v_13")
    val v_14 = new Point3D(0.235189f, 3.899839f, 99.353823f, "v_14")
    val v_15 = new Point3D(-2.000735f, 1.031470f, 105.496974f, "v_15")
    val v_16 = new Point3D(0.000000f, 0.000000f, 100.000000f, "v_16")
    val s_14 = new Trapezoid3D(v_1, v_3, v_5, v_15)
    val s_13 = new Trapezoid3D(v_2, v_4, v_6, v_16)
    val s_12 = new Trapezoid3D(v_7, v_8, v_10, v_9)
    val s_11 = new Trapezoid3D(v_9, v_10, v_12, v_11)
    val s_10 = new Trapezoid3D(v_5, v_7, v_9, v_11)
    val s_9 = new Trapezoid3D(v_5, v_6, v_8, v_7)
    val s_8 = new Trapezoid3D(v_3, v_4, v_6, v_5)
    val s_7 = new Trapezoid3D(v_6, v_8, v_10, v_12)
    val s_6 = new Trapezoid3D(v_11, v_12, v_14, v_13)
    val s_5 = new Trapezoid3D(v_1, v_2, v_16, v_15)
    val s_4 = new Trapezoid3D(v_13, v_14, v_16, v_15)
    val s_3 = new Trapezoid3D(v_5, v_11, v_13, v_15)
    val s_2 = new Trapezoid3D(v_1, v_3, v_4, v_2)
    val s_1 = new Trapezoid3D(v_6, v_12, v_14, v_16)
    //    val trapezoids = List(s_1, s_2, s_3, s_4, s_5, s_6, s_8, s_13, s_14)
    val trapezoids = List(s_1, s_2, s_3, s_4, s_5, s_6, s_7, s_8, s_9, s_10, s_11, s_12, s_13, s_14)
    val triangles = new ListBuffer[Triangle3D]
    val lines = new ListBuffer[Line3D]

    trapezoids.foreach(t => {
      triangles.appendAll(t.getComposingTriangles)
      lines.appendAll(t.getComposingVertices)
    })

    //    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(s_7.getComposingTriangles, s_7.getComposingVertices, 0.5)
    val afterRemovalLines = HiddenLineRemovalUtils.removeHiddenLines(triangles.toList, lines.toList, 0.5)

    afterRemovalLines.length must_== 7
  }
}