package akka.actors.homework

import akka.actor.{Actor, ActorRef, Props}

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

  /**
   * In theory, could be made more DRY. Not sure it's worth it.
   */

  override def receive: Receive = {
    case insert: Insert     => doInsert(insert)
    case contains: Contains => doContains(contains)
    case remove: Remove     => doRemove(remove)
  }

  private def doInsert(m: Insert): Unit = {
    if(m.elem == elem) m.requester ! OperationFinished(m.id)
    else {
      val newElement = context.actorOf(BinaryTreeNode.props(m.elem, initiallyRemoved = false))
      val sub = if(m.elem > elem) Right else Left

      subtrees.get(sub).fold({subtrees += (sub -> newElement); m.requester ! OperationFinished(m.id)})(_ ! m)
    }
  }

  private def doContains(m: Contains): Unit = {
    if (m.elem == elem) m.requester ! ContainsResult(m.id, !removed)
    else if(subtrees.isEmpty) m.requester ! ContainsResult(m.id, result = false)
    else {
      val sub = if(m.elem > elem) Right else Left
      subtrees.get(sub).fold(m.requester ! ContainsResult(m.id, result = false))(_ ! m)
    }
  }

  private def doRemove(m: Remove): Unit = {
    if (m.elem == elem) {
      removed = true
      m.requester ! OperationFinished(m.id)
    }
    else if(subtrees.isEmpty) m.requester ! OperationFinished(m.id)
    else {
      val sub = if(m.elem > elem) Right else Left
      subtrees.get(sub).fold(m.requester ! OperationFinished(m.id))(_ ! m)
    }
  }
}
