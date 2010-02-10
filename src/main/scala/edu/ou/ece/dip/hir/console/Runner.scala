package edu.ou.ece.dip.hir.console

import edu.ou.ece.dip.hir.controller.ProjectionController

object Runner {
  def main(args: Array[String]) = {
    val controller: ProjectionController = new ProjectionController
    controller.execute();
  }
}