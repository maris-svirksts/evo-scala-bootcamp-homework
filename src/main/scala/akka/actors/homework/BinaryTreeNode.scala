package akka.actors.homework

import akka.actor.{Actor, ActorRef, InternalActorRef, Props}

object BinaryTreeNode {
  private sealed trait Position

  private case object Left extends Position
  private case object Right extends Position

  def props(elem: Int, initiallyRemoved: Boolean): Props = Props(new BinaryTreeNode(elem, initiallyRemoved))
}

final class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet.Operation._
  import BinaryTreeSet.OperationReply._

  private var subtrees: Map[Position, ActorRef] = Map[Position, ActorRef]()
  private var removed: Boolean                  = initiallyRemoved

  /*
  - IntelliJ complains that the map is returning Unit. Not sure if that means that this is a hacky approach or no.
  It can be exchanged with match if needed.
  - Similar code, might be a good idea to refactor. Move Left / Right decisions to variable since actions do not differ.
  - InternalActorRef: was added by autocomplete, not sure what's the difference between it
  and ActorRef: both passed tests. Left it in for fun and, possibly, insightful comments from lectors.
   */

  override def receive: Receive = {
    case insert: Insert     => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove     => doRemove(remove)
    case _                  => ()
  }

  private def doInsert(m: Insert): Unit = {
    if(m.elem == elem) m.requester ! OperationFinished(m.id)
    else {
      val newElement = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))

      if(m.elem > elem) {
        subtrees.get(Right) match {
          case None =>
            subtrees += (Right -> newElement)
            m.requester ! OperationFinished(m.id)
          case Some(value) => value ! m
        }
      }
      else {
        subtrees.get(Left) match {
          case None =>
            subtrees += (Right -> newElement)
            m.requester ! OperationFinished(m.id)
          case Some(value) => value ! m
        }
      }
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) m.requester ! ContainsResult(m.id, !removed)
    else if(subtrees.isEmpty) m.requester ! ContainsResult(m.id, result = false)
    else {
      if(m.elem > elem) subtrees.get(Right).map {
        case ref: InternalActorRef => ref ! m
        case _ => m.requester ! ContainsResult(m.id, result = false)
      }
      else subtrees.get(Left).map {
        case ref: InternalActorRef => ref ! m
        case _ => m.requester ! ContainsResult(m.id, result = false)
      }
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    }
    else if(subtrees.isEmpty) m.requester ! OperationFinished(m.id)
    else {
      if(m.elem > elem) subtrees.get(Right).map {
        case ref: InternalActorRef => ref ! m
        case _ => m.requester ! OperationFinished(m.id)
      }
      else subtrees.get(Left).map {
        case ref: InternalActorRef => ref ! m
        case _ => m.requester ! OperationFinished(m.id)
      }
    }
  }
}
