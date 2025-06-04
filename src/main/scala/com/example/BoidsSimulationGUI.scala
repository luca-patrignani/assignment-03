package com.example

import java.awt.geom.Ellipse2D
import scala.swing.*
import scala.swing.event.*
import java.awt.{Color, Dimension, Graphics2D}
import javax.swing.BorderFactory
import scala.util.Random
import javax.swing.{Timer => SwingTimer}

object BoidsSimulationGUI extends SimpleSwingApplication:
  var boids: Seq[BoidState] = Seq.empty
  val space = Vector2d(1000, 500)
  val rules = BoidRules(avoidRadius = 20, perceptionRadius = 100, maxSpeed = 5, tSpace = space)
  val environmentCanvas = new Environment

  class Environment() extends Panel:
    preferredSize = Dimension(space.x.toInt, space.y.toInt)
    background = Color.white
    override def paintComponent(g: Graphics2D): Unit =
      g.clearRect(0, 0,preferredSize.width+10,preferredSize.height+10)
      g.setColor(Color.BLACK)
      boids.foreach(v => {
        val boid = new Ellipse2D.Double(v.position.x - 2, v.position.y - 2, 2 * 2, 2 * 2)
        g.draw(boid)
      })

  def render(newBoids: Seq[BoidState]): Unit =
    boids = newBoids
    println(s"update state")
    environmentCanvas.repaint()

  def top: Frame = new MainFrame:
    title = "Boids Simulation"
    preferredSize = Dimension(space.x.toInt,space.y.toInt + 200)

    val timer = new SwingTimer(16, _ => environmentCanvas.updateState()) // ~60 FPS

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
        new Label("Num Boids: "), numBoidsField,
        generateButton, startButton, stopButton
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
    listenTo(generateButton, startButton, stopButton,
      separationSlider, alignmentSlider, cohesionSlider)

    reactions += {
      case ButtonClicked(`generateButton`) =>
        generateButton.enabled = false
        startButton.enabled = true
        numBoidsLabel.text = s"Num. Boids: ${numBoidsField.text}"
        framerateLabel.text = "Framerate: 0"
        given random: Random = Random()
        boids = for i <- 1 to numBoidsField.text.toInt yield BoidState(
            Vector2d(random.nextInt(preferredSize.width), random.nextInt(preferredSize.height)),
            Vector2d(random.nextInt(3), random.nextInt(3)))
        environmentCanvas.updateState()
        println(s"Generate clicked with ${numBoidsField.text} boids")

      case ButtonClicked(`startButton`) =>
        startButton.enabled = false
        generateButton.enabled = false
        stopButton.enabled = true
        timer.start()
        println("Simulation started")

      case ButtonClicked(`stopButton`) =>
        stopButton.enabled = false
        generateButton.enabled = true
        startButton.enabled = true
        timer.stop()
        println("Simulation stopped")

      case ValueChanged(`separationSlider`) =>
        println(f"Separation: ${separationSlider.value / 10.0}%.1f")

      case ValueChanged(`alignmentSlider`) =>
        println(f"Alignment: ${alignmentSlider.value / 10.0}%.1f")

      case ValueChanged(`cohesionSlider`) =>
        println(f"Cohesion: ${cohesionSlider.value / 10.0}%.1f")
    }
