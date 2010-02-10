package edu.ou.ece.dip.hir.shape

import edu.ou.ece.dip.hir.SpecificationBase
import org.specs.runner.ScalaTest
import objectmother.HexahedronMother

class HexahedronSpec extends SpecificationBase with ScalaTest {
  "Should get composing triangles" in {
    HexahedronMother.regular.composingTriangles.length must_== 12
    HexahedronMother.regular.faces.length must_== 6
//    HexahedronMother.regular.composingVertices.length must_== 12
  }
}