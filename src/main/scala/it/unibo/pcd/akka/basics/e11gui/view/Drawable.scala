package it.unibo.pcd.akka.basics.e11gui.view

import java.awt.Graphics2D

// Trait for drawable elements
trait Drawable[G]:
  def draw(graphic: G): Unit

trait ElementFactory[G]:
  def createRectangle(x: Int, y: Int, width: Int, height: Int): Drawable[G]

trait DrawablePanel[G]:
  def addElement(element: Drawable[G]): Unit
  def whenClicked(listener: (Int, Int) => Unit): Unit
