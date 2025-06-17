package com.example

import akka.actor.typed.ActorRef
import com.example.BoidsRender.RenderMessage

import java.awt.geom.Ellipse2D
import java.awt.{Color, Dimension, Graphics2D}
import javax.swing.BorderFactory
import scala.swing.*
import scala.swing.event.*

case class BoidsSimulationGUI(renderRef: ActorRef[BoidsRender.RenderMessage], width: Int, height: Int)
    extends SimpleSwingApplication:
  var boids: Seq[Vector2d] = Seq.empty
  private val environmentCanvas = new Environment

  private val framerateLabel = new Label("Framerate: 0")

  private class Environment() extends Panel:
    preferredSize = Dimension(width, height)
    background = Color.white
    override def paintComponent(g: Graphics2D): Unit =
      g.clearRect(0, 0, preferredSize.width, preferredSize.height)
      g.setColor(Color.BLACK)
      boids.foreach { p =>
        val bShape = new Ellipse2D.Double(p.x - 2, p.y - 2, 2 * 2, 2 * 2)
        g.draw(bShape)
      }

  def render(newBoids: Seq[Vector2d], fps: Int): Unit =
    boids = newBoids
    framerateLabel.text = "Framerate: " + fps.toString
    environmentCanvas.repaint()

  def top: Frame = new MainFrame:
    title = "Boids Simulation"
    preferredSize = Dimension(width, height + 200)

    val numBoidsLabel = new Label("Num. Boids: 0")
    val numBoidsField = new TextField("0", 5)
    val generateButton = new Button("Generate")
    val startButton = new Button("Start") { enabled = false }
    val stopButton = new Button("Stop") { enabled = false }

    def createSlider(name: String): (Label, Slider) =
      val label = new Label(name)
      val slider = new Slider:
        min = 0
        max = 20
        value = 10
        majorTickSpacing = 5
        paintTicks = true
        paintLabels = true
      (label, slider)

    val (sepLabel, separationSlider) = createSlider("Separation")
    val (alignLabel, alignmentSlider) = createSlider("Alignment")
    val (cohLabel, cohesionSlider) = createSlider("Cohesion")

    val topPanel = new BoxPanel(Orientation.Vertical):
      contents += new FlowPanel(FlowPanel.Alignment.Left)(numBoidsLabel, framerateLabel)
      contents += new FlowPanel(FlowPanel.Alignment.Left)(
        new Label("Num Boids: "), numBoidsField, generateButton, startButton, stopButton
      )
      border = BorderFactory.createEmptyBorder(10, 10, 10, 10)

    val slidersPanel = new BoxPanel(Orientation.Horizontal):
      contents += new BoxPanel(Orientation.Vertical):
        contents += sepLabel
        contents += separationSlider
      contents += Swing.HStrut(20)
      contents += new BoxPanel(Orientation.Vertical):
        contents += alignLabel
        contents += alignmentSlider
      contents += Swing.HStrut(20)
      contents += new BoxPanel(Orientation.Vertical):
        contents += cohLabel
        contents += cohesionSlider
      border = BorderFactory.createEmptyBorder(10, 10, 10, 10)

    contents = new BorderPanel:
      layout(topPanel) = BorderPanel.Position.North
      layout(environmentCanvas) = BorderPanel.Position.Center
      layout(slidersPanel) = BorderPanel.Position.South

    // ACTION LISTENER
    listenTo(generateButton, startButton, stopButton, separationSlider, alignmentSlider, cohesionSlider)

    reactions += {
      case ButtonClicked(`generateButton`) =>
        generateButton.enabled = false
        startButton.enabled = true
        numBoidsLabel.text = s"Num. Boids: ${numBoidsField.text}"
        framerateLabel.text = "Framerate: 0"
        renderRef ! BoidsRender.RenderMessage.GenerateBoids(
          numBoidsField.text.toInt,
          separationSlider.value / 10.0,
          alignmentSlider.value / 10.0,
          cohesionSlider.value / 10.0
        )
        println(s"Generate clicked with ${numBoidsField.text} boids")

      case ButtonClicked(`startButton`) =>
        startButton.enabled = false
        generateButton.enabled = false
        stopButton.enabled = true
        renderRef ! BoidsRender.RenderMessage.StartSimulation
        println("Simulation started")

      case ButtonClicked(`stopButton`) =>
        stopButton.enabled = false
        generateButton.enabled = true
        startButton.enabled = true
        renderRef ! BoidsRender.RenderMessage.StopSimulation
        println("Simulation stopped")

      case ValueChanged(`separationSlider` | `alignmentSlider` | `cohesionSlider`) =>
        renderRef ! BoidsRender.RenderMessage.UpdateParameter(
          separationSlider.value.toDouble / 10,
          alignmentSlider.value.toDouble / 10,
          cohesionSlider.value.toDouble / 10
        )

    }
