package it.unibo.pcd.akka.basics.e11gui.view

import java.awt.{Color, Graphics2D}

// Implementation of a Drawable Rectangle
case class DrawableRectangle(x: Int, y: Int, width: Int, height: Int, color: Color) extends Drawable[Graphics2D]:
  override def draw(graphic: Graphics2D): Unit =
    graphic.setColor(color)
    graphic.fillRect(x, y, width, height)
