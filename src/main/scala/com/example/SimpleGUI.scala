package com.example

import java.awt.{BorderLayout, Canvas, Color, Dimension, Graphics}
import javax.swing.{JButton, JFrame, JPanel, SwingUtilities}

class SimpleGUI(val width: Int, val height: Int):
  private val elementWidth = 10
  private val frame = JFrame()
  private val canvas = Environment()
  private val button = JButton("Bottone")
  frame.setSize(width, height)
  frame.setVisible(true)
  canvas.setVisible(true)
  frame.setLocationRelativeTo(null)
  frame.setLayout(BorderLayout())
  canvas.setSize(width, height)
  frame.add(canvas)
  button.setVisible(true)
  button.setSize(50,20)
  frame.add(button)


  def render(elements: List[(Int, Int)]): Unit = SwingUtilities.invokeLater { () =>
    canvas.elements = elements
    canvas.invalidate()
    canvas.repaint()
  }

  private class Environment() extends JPanel:
    var elements: List[(Int, Int)] = List.empty
    override def getPreferredSize = new Dimension(SimpleGUI.this.width, SimpleGUI.this.height)
    override def paintComponent(graphics: Graphics): Unit =
      graphics.clearRect(0, 0, SimpleGUI.this.width, SimpleGUI.this.height)
      graphics.setColor(Color.BLACK)
      elements.foreach((x, y) => graphics.drawOval(x, y, elementWidth, elementWidth))
