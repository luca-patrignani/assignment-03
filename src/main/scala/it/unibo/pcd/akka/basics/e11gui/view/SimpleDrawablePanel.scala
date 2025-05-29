package it.unibo.pcd.akka.basics.e11gui.view

import java.awt.Graphics2D
import scala.swing.event.MousePressed
import scala.swing.{Panel, Swing}

// Custom panel for drawing elements
class SimpleDrawablePanel extends Panel with DrawablePanel[Graphics2D] {
  private var elements: List[Drawable[Graphics2D]] = List()
  listenTo(mouse.clicks)
  // Add a drawable element to the panel
  def addElement(element: Drawable[Graphics2D]): Unit =
    elements = element :: elements
    Swing.onEDT(repaint())

  override def whenClicked(listener: (Int, Int) => Unit): Unit =
    reactions += { case MousePressed(_, point, _, _, _) =>
      listener(point.x, point.y)
    }
  // Custom painting of all elements
  override def paintComponent(g: Graphics2D): Unit =
    super.paintComponent(g)
    elements.foreach(_.draw(g))
}
