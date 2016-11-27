package naoh.lamp

import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.transform.RewriteRule

class Xe(
  private val head: Lamp,
  private val chain: Option[Xe]
) {
  def \(label: String): Xe = \(Lamp(_.label == label))

  private def \(lamp: Lamp): Xe = new Xe(lamp, Some(this))

  def \(that: Xe): Xe = at(that)

  def on(that: Xe): Xe = on(that.lamp)

  def at(that: Xe): Xe = new Xe(that.head, Some(that.chain.fold(this)(at)))

  def remove(src: Node): Node =
    toRule(Replace(NodeSeq.Empty, head))(src)

  def set(rep: NodeSeq)(src: Node): Node =
    toRule(Replace(rep, head))(src)

  def transform(rep: Node => NodeSeq)(src: Node): Node =
    toRule(Transform(rep, head))(src)

  def append(rep: NodeSeq)(src: Node): Node =
    toRule(Append(rep, head))(src)

  def find(src: Node): Seq[Node] =
    chain.fold[Seq[Node]](Seq(src).filter(head.apply)) { xe =>
      xe.find(src).flatMap(_.child.filter(head.apply))
    }

  def attr(key: String)(value: String => Boolean): Xe =
    on(Lamp(_.attribute(key).exists(_.exists(n => value(n.text)))))

  private def on(lamp: Lamp): Xe = new Xe(head && lamp, chain)

  def text(str: String => Boolean): Xe =
    on(Lamp(node => str(node.text)))

  private def toRule(c: RewriteRule): RewriteRule =
    chain.fold(c)(xe => xe.toRule(Chain(c, xe.head)))

  private def lamp: Lamp = Lamp { node =>
    chain.fold(head(node)) { xe =>
      Seq(node).filter(xe.lamp.apply)
        .flatMap(_.child).exists(head.apply)
    }
  }
}

object Xe {
  def \(label: String): Xe = new Xe(Lamp(_.label == label), None)

  def \(lamp: Lamp): Xe = new Xe(lamp, None)
}

case class Lamp(cond: Node => Boolean) {

  def &&(that: Lamp): Lamp = Lamp {
    node => cond(node) && that(node)
  }

  def apply(node: Node): Boolean = cond(node)
}
