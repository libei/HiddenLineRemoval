package edu.ou.ece.dip.hlr.shape

import edu.ou.ece.dip.hlr.SpecificationBase
import objectmother.HexahedronMother

class HexahedronSpec extends SpecificationBase {
  "Should get composing triangles" in {
    HexahedronMother.regular.composingTriangles.length must_== 12
    HexahedronMother.regular.faces.length must_== 6
//    HexahedronMother.regular.composingVertices.length must_== 12
  }
}