package naoh.lamp

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.transform.RewriteRule

case class Xe(
  head: Lamp,
  chain: Seq[Lamp]
) {
  def \(label: String): Xe = \(Lamp(_.label == label, label))

  def \(lamp: Lamp): Xe = Xe(lamp, head +: chain)

  def on(lamp: Lamp): Xe = Xe(head * lamp, chain)

  def set(rep: NodeSeq)(src: Elem): Node = {
    chain.foldLeft[RewriteRule](Leaf(rep, head))(ChainRule)(src)
  }
}

object Xe {
  def \(label: String): Xe = Xe(Lamp(_.label == label, label), Seq.empty)
}

case class Lamp(cond: Node => Boolean, desc: String) {

  def *(that: Lamp): Lamp = Lamp(
    node => cond(node) && that(node),
    s"($desc && ${that.desc})")

  def apply(node: Node): Boolean = cond(node)
}

case class Leaf(node: NodeSeq, lamp: Lamp) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case elem@Elem(_, _, _, _, _*) if lamp(elem) =>
      node
    case other =>
      println(s"X L ${lamp.desc} # ${other.label}")
      other
  }
}

case class ChainRule(nextRule: RewriteRule, lamp: Lamp) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case elem@Elem(prefix, label, attr, scope, nodes@_*) if lamp(elem) =>
      println(s"O CR ${lamp.desc} # $label")
      Elem(prefix, label, attr, scope, true, nodes.flatMap(nextRule): _*)
    case other =>
      println(s"X CR ${lamp.desc} # ${other.label}")
      other
  }
}