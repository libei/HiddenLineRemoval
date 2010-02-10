package edu.ou.ece.dip.hlr.console

import edu.ou.ece.dip.hlr.controller.ProjectionController

object Runner {
  def main(args: Array[String]) = {
    val controller: ProjectionController = new ProjectionController
    controller.execute();
  }
}