package it.unibo.pcd.akka.basics.e01hello;
import akka.actor.typed.ActorSystem;
public class Main {
    public static void main(String[] args) {
        ActorSystem<HelloActorJava.Greet> system = ActorSystem.create(HelloActorJava.create(), "hello-world");
        system.tell(new HelloActorJava.Greet("Akka Typed", system.ignoreRef()));
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        system.terminate();
    }
}
