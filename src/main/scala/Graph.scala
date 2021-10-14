class Graph[N](adjList: Map[N, Seq[N]]) {

  def nodes: List[N] = (adjList.keys.toSet ++ adjList.values.flatten.toSet).toList

  def edges: List[(N, N)] = adjList.map {
    case (node, children) => {
      children.map(child => (node, child))
    }
  }.flatten.toList

  def addNode(n: N): Graph[N] = new Graph(adjList + (n -> List()))

  def addEdge(from: N, to: N): Graph[N] = {
    val fromChildren = to +: children(from)
    val g = new Graph(adjList + (from -> fromChildren))

    if(g.nodes.contains(to)) g else g.addNode(to)

  }

  def removeEdge(from: N, to: N): Graph[N] = {
    val fromChildren = children(from) diff Seq(to)
    new Graph(adjList + (from -> fromChildren))
  }

  def children(node: N): Seq[N] = {
    adjList.getOrElse(node, Nil)
  }

  def parents(node: N): List[N] = {
    edges.filter(_._2 == node).map(_._1)
  }
}

object Graph {
  def apply[N](adjList: Map[N, Seq[N]]): Graph[N] = new Graph(adjList)
  def apply[N](): Graph[N] = new Graph(Map[N, List[N]]())
}