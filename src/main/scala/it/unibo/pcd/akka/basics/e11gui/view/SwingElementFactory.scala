package it.unibo.pcd.akka.basics.e11gui.view

import java.awt.Color
import scala.swing.Graphics2D

class SwingElementFactory extends ElementFactory[Graphics2D]:
  override def createRectangle(x: Int, y: Int, w: Int, h: Int): Drawable[Graphics2D] =
    DrawableRectangle(x, y, w, h, Color.BLACK)
