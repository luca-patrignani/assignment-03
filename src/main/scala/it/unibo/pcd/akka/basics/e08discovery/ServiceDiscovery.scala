package it.unibo.pcd.akka.basics.e08discovery
import akka.actor.typed.ActorRef
import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.receptionist.Receptionist
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.Behaviors
import it.unibo.pcd.akka.basics.e08discovery.IDGenerator.RequestId

import java.util.UUID
import javax.swing.plaf.basic.BasicTableHeaderUI
import scala.language.postfixOps

object IDGenerator:
  case class RequestId(self: ActorRef[String])
  val key = ServiceKey[RequestId]("id-generator")
  def apply(): Behavior[RequestId] = Behaviors.setup[RequestId]: ctx =>
    ctx.system.receptionist ! Receptionist.register(key, ctx.self) // an actor add themself to the service
    Behaviors.receiveMessage:
      case (RequestId(sender)) =>
        sender ! UUID.randomUUID().toString
        ctx.log.info("request received!")
        Behaviors.same

object UsingSubscribe: // Foo actors, when it is created, it uses the receptionist to be subscribed to new IDGenerator actor
  def apply(): Behavior[Nothing] = Behaviors
    .setup[Receptionist.Listing]: ctx =>
      val myPrinter = ctx.spawnAnonymous(Printer())
      ctx.system.receptionist ! Receptionist.Subscribe(key = IDGenerator.key, ctx.self)
      Behaviors.receiveMessagePartial[Receptionist.Listing]:
        case IDGenerator.key.Listing(lst) =>
          ctx.log.info("new generator (subscribe)" + lst.size)
          lst.foreach(_ ! RequestId(myPrinter))
          Behaviors.same
    .narrow

object UsingFind: // Foo actors, uses Find, i.e., single query
  def apply(): Behavior[Nothing] = Behaviors
    .setup[Receptionist.Listing] { ctx =>
      val myPrinter = ctx.spawnAnonymous(Printer())
      ctx.system.receptionist ! Receptionist.Find(key = IDGenerator.key, ctx.self)
      Behaviors.receiveMessagePartial[Receptionist.Listing] { case IDGenerator.key.Listing(lst) =>
        ctx.log.info("new generator (find)" + lst.size)
        lst.foreach(_ ! RequestId(myPrinter))
        Behaviors.stopped
      }
    }
    .narrow
object Printer:
  def apply(): Behavior[String] = Behaviors.receive:
    case (ctx, msg) =>
      ctx.log.info(msg)
      Behaviors.same
@main def checkDiscovery: Unit =
  enum Command:
    case Start
  val system = ActorSystem.create(
    Behaviors
      .setup[Command]: ctx =>
        ctx.spawnAnonymous(IDGenerator())
        ctx.spawnAnonymous(UsingSubscribe())
        ctx.spawnAnonymous(UsingFind())
        Behaviors.receiveMessage:
          case Command.Start =>
            ctx.spawnAnonymous(IDGenerator())
            Behaviors.same
      .narrow,
    "discovery-check"
  )
  Thread.sleep(300)
  system ! Command.Start
  Thread.sleep(500)
  system.terminate()
