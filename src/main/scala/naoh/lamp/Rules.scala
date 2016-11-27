package naoh.lamp

import scala.xml.Elem
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.transform.RewriteRule


case class Append(append: NodeSeq, lamp: Lamp) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case elem@Elem(prefix, label, attr, scope, nodes@_*) if lamp(elem) =>
      Elem(prefix, label, attr, scope, true, nodes ++ append: _*)
    case other =>
      other
  }
}

case class Replace(node: NodeSeq, lamp: Lamp) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case elem@Elem(_, _, _, _, _*) if lamp(elem) => node
    case other => other
  }
}

case class Transform(rule: Node => NodeSeq, lamp: Lamp) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case elem@Elem(_, _, _, _, _*) if lamp(elem) => rule(elem)
    case other => other
  }
}

case class Chain(nextRule: RewriteRule, lamp: Lamp) extends RewriteRule {
  override def transform(n: Node): Seq[Node] = n match {
    case elem@Elem(prefix, label, attr, scope, nodes@_*) if lamp(elem) =>
      Elem(prefix, label, attr, scope, true, nodes.flatMap(nextRule): _*)
    case other =>
      other
  }
}