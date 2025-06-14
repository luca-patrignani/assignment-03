package com.example

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext
import com.example.BoidsRender.RenderMessage

import java.awt.geom.Ellipse2D
import scala.swing.*
import scala.swing.event.*
import java.awt.{Color, Dimension, Graphics2D}
import javax.swing.BorderFactory
import scala.util.Random
import javax.swing.Timer as SwingTimer

case class BoidsSimulationGUI(renderRef: ActorRef[BoidsRender.RenderMessage]) extends SimpleSwingApplication:
  var boids: Seq[Vector2d] = Seq.empty
  val space = Vector2d(1000, 500)
  val environmentCanvas = new Environment

  class Environment() extends Panel:
    preferredSize = Dimension(space.x.toInt, space.y.toInt)
    background = Color.white
    override def paintComponent(g: Graphics2D): Unit =
      g.clearRect(0, 0, preferredSize.width + 10, preferredSize.height + 10)
      g.setColor(Color.BLACK)
      boids.foreach { p =>
        val boid = new Ellipse2D.Double(p.x - 2, p.y - 2, 2 * 2, 2 * 2)
        g.draw(boid)
      }

  def render(newBoids: Seq[Vector2d]): Unit =
    boids = newBoids
    environmentCanvas.repaint()

  def top: Frame = new MainFrame:
    title = "Boids Simulation"
    preferredSize = Dimension(space.x.toInt, space.y.toInt + 200)

    val numBoidsField = new TextField("0", 5)
    val generateButton = new Button("Generate")
    val startButton = new Button("Start") { enabled = false }
    val stopButton = new Button("Stop") { enabled = false }

    val numBoidsLabel = new Label("Num. Boids: 0")
    val framerateLabel = new Label("Framerate: 0")

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

      case ValueChanged(`separationSlider`) =>
        renderRef ! BoidsRender.RenderMessage.UpdateParameter(
          separationSlider.value.toDouble,
          alignmentSlider.value.toDouble,
          cohesionSlider.value.toDouble
        )
        println(f"Separation: ${separationSlider.value / 10.0}%.1f")

      case ValueChanged(`alignmentSlider`) =>
        renderRef ! BoidsRender.RenderMessage.UpdateParameter(
          separationSlider.value.toDouble,
          alignmentSlider.value.toDouble,
          cohesionSlider.value.toDouble
        )
        println(f"Alignment: ${alignmentSlider.value / 10.0}%.1f")

      case ValueChanged(`cohesionSlider`) =>
        renderRef ! BoidsRender.RenderMessage.UpdateParameter(
          separationSlider.value.toDouble,
          alignmentSlider.value.toDouble,
          cohesionSlider.value.toDouble
        )
        println(f"Cohesion: ${cohesionSlider.value / 10.0}%.1f")

    }
