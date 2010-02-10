package edu.ou.ece.dip.hlr.controller

import edu.ou.ece.dip.hlr.view.ProjectionView
import edu.ou.ece.dip.hlr.shape.{Triangle3D, Point3D}

class ProjectionController {
  def execute() {
    val projectionView = new ProjectionView()
    projectionView.display(new Triangle3D(new Point3D(5, 3, 0),
      new Point3D(4, 8, 0),
      new Point3D(2, 9, 0)))
  }

}