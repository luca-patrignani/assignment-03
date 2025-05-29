package it.unibo.pcd.akka.basics.e02oopstyle;

import akka.actor.typed.ActorRef;
import akka.actor.typed.Behavior;
import akka.actor.typed.javadsl.AbstractBehavior;
import akka.actor.typed.ActorSystem;
import akka.actor.typed.javadsl.ActorContext;
import akka.actor.typed.javadsl.Behaviors;
import akka.actor.typed.javadsl.Receive;
import java.time.Duration;

public class PingPongerJava extends AbstractBehavior<PingPongerJava.PingPongJava> {
    // Enumeration equivalent using classes in Java
    public static abstract class PingPongJava {
        private PingPongJava() {}  // Make it non-instantiable
        public static final class Pong extends PingPongJava {
            public final ActorRef<Ping> replyTo;

            public Pong(ActorRef<Ping> replyTo) {
                this.replyTo = replyTo;
            }
        }

        public static final class Ping extends PingPongJava {
            public final ActorRef<Pong> replyTo;

            public Ping(ActorRef<Pong> replyTo) {
                this.replyTo = replyTo;
            }
        }
    }

    private int bounces;

    public PingPongerJava(ActorContext<PingPongerJava.PingPongJava> context, int bounces) {
        super(context);
        this.bounces = bounces;
        context.getLog().info("Hello. My path is: " + context.getSelf().path());
    }

    @Override
    public Receive<PingPongJava> createReceive() {
        return newReceiveBuilder()
                .onMessage(PingPongJava.Pong.class, this::onPong)
                .onMessage(PingPongJava.Ping.class, this::onPing)
                .build();
    }

    private Behavior<PingPongJava> onPong(PingPongJava.Pong message) {
        bounces--;
        if (bounces < 0) {
            getContext().getLog().info("I got tired of pingpong-ing. Bye bye.");
            return Behaviors.stopped();
        } else {
            getContext().getLog().info("Pong");
            getContext().scheduleOnce(
                Duration.ofSeconds(1), 
                message.replyTo.unsafeUpcast(), 
                new PingPongJava.Ping(getContext().getSelf().unsafeUpcast())
            );
            return this;
        }
    }

    private Behavior<PingPongJava> onPing(PingPongJava.Ping message) {
        bounces--;
        if (bounces < 0) {
            getContext().getLog().info("I got tired of pingpong-ing. Bye bye.");
            return Behaviors.stopped();
        } else {
            getContext().getLog().info("Ping");
            getContext().scheduleOnce(Duration.ofSeconds(1), message.replyTo.unsafeUpcast(), new PingPongJava.Pong(getContext().getSelf().unsafeUpcast()));
            return this;
        }
    }

    public static Behavior<PingPongJava> create(int bounces) {
        return Behaviors.setup(context -> new PingPongerJava(context, bounces));
    }

    public static void main(String[] args) {
        ActorRef<PingPongJava> pingPonger = ActorSystem.create(PingPongerJava.create(10), "pingPonger");
        pingPonger.tell(new PingPongJava.Ping(pingPonger.unsafeUpcast()));
    }
}