import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object SortUtils {
  def sort(dependencies: Map[String, Seq[String]], parallel: Boolean = false): Seq[String] = {
    var graph: Graph[String] = Graph(dependencies)
    val result = mutable.Stack[String]()
    val queue = mutable.Stack[String]()
    val visited = graph.nodes.map(node => (node -> new AtomicBoolean())).toMap

    import scala.concurrent.ExecutionContext.Implicits.global

    def init(): Unit = {
      for (node <- graph.nodes) {
        if (graph.parents(node).isEmpty) {
          visited(node).set(true)
          queue.push(node)
        }
      }

      var futures: Seq[Future[Unit]] = Seq()

      while (queue.nonEmpty || !Future.sequence(futures).isCompleted) {
        if (parallel) {
          futures = futures :+ Future {
            doTopSortProcess()
          }
        } else {
          doTopSortProcess()
        }
      }

      if (parallel) {Await.result(Future.sequence(futures), Duration.Inf)}

      for ((node, state) <- visited) {
        if (!state.get()) {
          throw new RuntimeException(f"Node $node is not visited so not a DAG")
        }
      }
    }

    def doTopSortProcess(): Unit = {
      if (queue.nonEmpty) {
        val node: String = queue.pop()
        result.push(node)
        for (child <- graph.children(node)) {
          graph = graph.removeEdge(node, child)
          if (graph.parents(child).isEmpty) {
            val isVisited = visited(child).getAndSet(true)
            if (!isVisited) {
              queue.push(child)
            }
          }
        }
      }
    }

    init()
    result.reverse.toSeq
  }
}


