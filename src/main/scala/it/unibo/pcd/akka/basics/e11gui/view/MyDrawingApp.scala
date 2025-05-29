package it.unibo.pcd.akka.basics.e11gui.view

import java.awt.Color
import scala.swing.{Dimension, Frame, MainFrame, SimpleSwingApplication}

// Main frame setup
object MyDrawingApp extends SimpleSwingApplication:
  val panel = SimpleDrawablePanel()
  def top: Frame = new MainFrame:
    title = "Drawable Panel Example"
    preferredSize = new Dimension(400, 300)
    contents = panel

    // Adding some rectangles to the panel

  panel.addElement(DrawableRectangle(10, 10, 100, 50, Color.red))
  panel.addElement(DrawableRectangle(50, 70, 150, 100, Color.blue))
