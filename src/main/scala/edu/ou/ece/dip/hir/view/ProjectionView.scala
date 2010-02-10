package edu.ou.ece.dip.hir.view

import java.awt.{Frame}
import java.awt.event._
import javax.media.opengl._
import com.sun.opengl.util.Animator
import glu.GLU
import edu.ou.ece.dip.hir.shape.Triangle3D

class ProjectionView extends GLEventListener with KeyListener {
  val frame = new Frame("Hello World");
  val canvas: GLCanvas = new GLCanvas()
  val animator: Animator = new Animator(canvas);
  frame.add(canvas)
  frame.setSize(300, 300)
  frame.setUndecorated(true)
  frame.setExtendedState(Frame.MAXIMIZED_BOTH)
  frame.addWindowListener(new WindowAdapter {
    override def windowClosing(e: WindowEvent): Unit = {
      exit
    }
  })
  canvas.addGLEventListener(this)
  var glu = new GLU()
  var triangle: Triangle3D = null

  def display(triangle: Triangle3D) {
    this.triangle = triangle
    frame.setVisible(true)
    animator.start();
    canvas.requestFocus
  }

  def init(drawable: GLAutoDrawable) = {
    var gl: GL = drawable.getGL
    gl.glShadeModel(GL.GL_SMOOTH)
    gl.glClearColor(0.0f, 0.0f, 0.0f, 0.0f)
    gl.glClearDepth(1.0)
    gl.glEnable(GL.GL_DEPTH_TEST)
    gl.glDepthFunc(GL.GL_LEQUAL)
    gl.glHint(GL.GL_PERSPECTIVE_CORRECTION_HINT, GL.GL_NICEST)
  }

  def displayChanged(p1: GLAutoDrawable, p2: Boolean, p3: Boolean) = {}

  def reshape(drawable: GLAutoDrawable, x: Int, y: Int, width: Int, height: Int) = {
    var gl: GL = drawable.getGL

    var adjustedHeight = height
    if (adjustedHeight <= 0) {adjustedHeight = 1}

    var h: Double = width.asInstanceOf[Double] / adjustedHeight.asInstanceOf[Double]
    gl.glMatrixMode(GL.GL_PROJECTION)
    gl.glLoadIdentity
    glu.gluPerspective(50.0, h, 1.0, 1000.0)
    gl.glMatrixMode(GL.GL_MODELVIEW)
    gl.glLoadIdentity
  }

  def display(drawable: GLAutoDrawable) = {
    val gl: GL = drawable.getGL()
    gl.glClear(GL.GL_COLOR_BUFFER_BIT)
    gl.glClear(GL.GL_DEPTH_BUFFER_BIT)
    gl.glLoadIdentity
    gl.glTranslatef(0.0f, 0.0f, -5.0f)
    gl.glBegin(GL.GL_TRIANGLES)

    gl.glVertex2f(triangle.cornerA.x.toFloat / 10.0f, triangle.cornerA.y.toFloat / 10.0f)
    gl.glVertex2f(triangle.cornerB.x.toFloat / 10.0f, triangle.cornerB.y.toFloat / 10.0f)
    gl.glVertex2f(triangle.cornerC.x.toFloat / 10.0f, triangle.cornerC.y.toFloat / 10.0f)

    gl.glEnd
  }

  def keyPressed(e: KeyEvent) = {
    exit(0)
  }

  def keyTyped(e: KeyEvent) = {
  }

  def keyReleased(e: KeyEvent) = {
  }

}
